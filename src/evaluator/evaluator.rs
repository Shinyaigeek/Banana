use crate::evaluator::object::object::{Bool, Float, Function, Integer, Null, Object};
use crate::evaluator::variable::variable::{Environment, VariableValue};
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{
    BlockStatement, Expression, IfStatement, InfixOperator, Literal, LiteralType, Node, Parser,
    PrefixOperator, Statement, StatementType,
};
use crate::parser::tokenizer::tokenizer::Tokens;

pub fn evaluate(node: Node, environment: &mut Environment, break_flag: &mut bool) -> Object {
    match node {
        Node::Program(program) => evaluate_statements(program.body, environment),
        Node::Statement(statement) => match statement.statement {
            StatementType::Expression(expression) => {
                handle_expression(Box::new(expression), environment)
            }
            StatementType::IfStatement(if_statement) => {
                let mut environment = environment.extend();
                handle_if_statement(if_statement, &mut environment)
            }
            StatementType::VariableDeclaration(variable_declaration) => {
                let value = handle_expression(Box::new(variable_declaration.init), environment);
                environment.set(
                    variable_declaration.identifier.value,
                    // TODO
                    VariableValue::Object(value.clone()),
                );
                value
            }
            StatementType::FunctionDeclarationStatement(function_declaration_statement) => {
                let body = match *function_declaration_statement.body {
                    StatementType::BlockStatement(block_statement) => block_statement.body,
                    _ => panic!("function's body should be BlockStatement"),
                };
                let arguments = {
                    let mut res: Vec<String> = vec![];
                    for argument in function_declaration_statement.arguments {
                        res.push(argument.value.clone());
                    }
                    res
                };
                let value = Object::Function(Function { arguments, body });
                environment.set(
                    function_declaration_statement.identifier.value,
                    // TODO
                    VariableValue::Object(value.clone()),
                );
                value
            }
            StatementType::ReturnStatement(return_statement) => {
                *break_flag = true;
                return handle_expression(Box::new(return_statement.arguments), environment);
            }
            _ => panic!("{:?}", statement),
        },
        Node::Expression(expression) => handle_expression(Box::new(expression), environment),
        _ => panic!(""),
    }
}

pub fn evaluate_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result: Option<Object> = None;
    let mut break_flag = false;
    for statement in statements {
        result = Some(evaluate(
            statement_to_node(statement),
            environment,
            &mut break_flag,
        ));
        if break_flag {
            break;
        }
    }
    match result {
        Some(result) => result,
        None => panic!(""),
    }
}

pub fn statement_to_node(statement: Statement) -> Node {
    Node::Statement(statement)
}

pub fn handle_if_statement(if_statement: IfStatement, environment: &mut Environment) -> Object {
    let test = handle_expression(if_statement.test, environment);
    let test = match test {
        Object::Bool(bool) => bool,
        _ => panic!("if's test should be boolean"),
    };
    if test.value {
        evaluate_statements(if_statement.consequents, environment)
    } else {
        let alternate = if_statement.alternate;
        match *alternate {
            Some(alternate) => match alternate {
                StatementType::IfStatement(if_statement) => {
                    handle_if_statement(if_statement, environment)
                }
                // * else
                StatementType::BlockStatement(block_statement) => {
                    handle_block_statement(block_statement, environment)
                }
                _ => panic!("if statements' alternate should be if_statement or block statement"),
            },
            // * else
            None => Object::Null(Null {}),
        }
    }
}

pub fn handle_block_statement(
    block_statement: BlockStatement,
    environment: &mut Environment,
) -> Object {
    evaluate_statements(block_statement.body, environment)
}

pub fn handle_expression(expression: Box<Expression>, environment: &mut Environment) -> Object {
    match *expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(int) => {
                let int: i32 = int.value.parse().unwrap();
                Object::Integer(Integer { value: int })
            }

            Literal::Float(float) => {
                let float: f32 = float.value.parse().unwrap();
                Object::Float(Float { value: float })
            }

            Literal::Boolean(boolean) => {
                let boolean: bool = boolean.value.parse().unwrap();
                Object::Bool(Bool { value: boolean })
            }
            _ => panic!(""),
        },
        Expression::Identifier(identifier) => {
            let value = environment.get(identifier.value);
            match value {
                VariableValue::Object(obj) => obj.clone(),
            }
        }
        Expression::PrefixExpression(prefix_expression) => {
            let obj = handle_expression(prefix_expression.right, environment);
            match prefix_expression.operator {
                PrefixOperator::MINUS => {
                    handle_prefix_literal(obj, PrefixOperator::MINUS, environment)
                }
                PrefixOperator::EXCLAMATION => {
                    handle_prefix_literal(obj, PrefixOperator::EXCLAMATION, environment)
                }
                _ => panic!("prefix operator should be - or !"),
            }
        }
        Expression::InfixExpression(infix_expression) => {
            let left = handle_expression(infix_expression.left, environment);
            let right = handle_expression(infix_expression.right, environment);
            handle_infix_expression(left, right, infix_expression.operator, environment)
        }
        Expression::CallExpression(call_expression) => {
            let call = environment.get(call_expression.callee.value);
            let call = match call {
                VariableValue::Object(object) => object,
            };
            let call = match call {
                Object::Function(function) => function,
                _ => panic!(
                    "function value should be Object::Function, but got {:?}",
                    call
                ),
            };
            let call = call.clone();
            let mut environment_with_args = environment.extend();
            for idx in 0..(call.arguments.len()) {
                let value = handle_expression(
                    Box::new(call_expression.arguments[idx].clone()),
                    environment,
                );
                let value = VariableValue::Object(value);
                environment_with_args.set(call.arguments[idx].clone(), value.clone());
            }

            evaluate_statements(call.body.clone(), &mut environment_with_args)
        }
        _ => panic!(""),
    }
}

pub fn handle_prefix_literal(
    obj: Object,
    prefix: PrefixOperator,
    environment: &mut Environment,
) -> Object {
    if prefix == PrefixOperator::MINUS {
        match obj {
            Object::Integer(int) => Object::Integer(Integer {
                value: -1 * int.value,
            }),
            Object::Float(float) => Object::Float(Float {
                value: -1_f32 * float.value,
            }),
            Object::Bool(bool) => Object::Integer(Integer {
                value: if bool.value { -1 } else { 0 },
            }),
            _ => panic!(
                "prefix literal - should be used with integer or bool, but got {:?}",
                obj
            ),
        }
    } else {
        match obj {
            Object::Integer(int) => Object::Bool(Bool {
                value: int.value == 0,
            }),
            Object::Float(float) => Object::Bool(Bool {
                value: float.value == 0.0,
            }),
            Object::Bool(bool) => Object::Bool(Bool { value: !bool.value }),
            Object::Null(_) => Object::Bool(Bool { value: true }),
            _ => panic!(""),
        }
    }
}

pub fn handle_infix_expression(
    left: Object,
    right: Object,
    operator: InfixOperator,
    environment: &mut Environment,
) -> Object {
    match operator {
        InfixOperator::PLUS => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Integer(Integer {
                    value: left.value + right.value,
                }),
                Object::Float(right) => Object::Float(Float {
                    value: (left.value as f32) + right.value,
                }),
                _ => panic!("+ should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Float(Float {
                    value: left.value + (right.value as f32),
                }),
                Object::Float(right) => Object::Float(Float {
                    value: left.value + right.value,
                }),
                _ => panic!("+ should be plused with number and number"),
            },
            _ => panic!("+ should be plused with number and number"),
        },
        InfixOperator::MINUS => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Integer(Integer {
                    value: left.value - right.value,
                }),
                Object::Float(right) => Object::Float(Float {
                    value: (left.value as f32) - right.value,
                }),
                _ => panic!("- should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Float(Float {
                    value: left.value - (right.value as f32),
                }),
                Object::Float(right) => Object::Float(Float {
                    value: left.value - right.value,
                }),
                _ => panic!("- should be plused with number and number"),
            },
            _ => panic!("- should be plused with number and number"),
        },
        InfixOperator::ASTERISK => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Integer(Integer {
                    value: left.value * right.value,
                }),
                Object::Float(right) => Object::Float(Float {
                    value: (left.value as f32) * right.value,
                }),
                _ => panic!("* should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Float(Float {
                    value: left.value * (right.value as f32),
                }),
                Object::Float(right) => Object::Float(Float {
                    value: left.value * right.value,
                }),
                _ => panic!("* should be plused with number and number"),
            },
            _ => panic!("* should be plused with number and number"),
        },
        InfixOperator::SLASH => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Float(Float {
                    value: (left.value as f32) / (right.value as f32),
                }),
                Object::Float(right) => Object::Float(Float {
                    value: (left.value as f32) / right.value,
                }),
                _ => panic!("/ should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Float(Float {
                    value: left.value / (right.value as f32),
                }),
                Object::Float(right) => Object::Float(Float {
                    value: left.value / right.value,
                }),
                _ => panic!("/ should be plused with number and number"),
            },
            _ => panic!("/ should be plused with number and number"),
        },
        InfixOperator::GRATER => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value > right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) > right.value,
                }),
                _ => panic!("> should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value > (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value > right.value,
                }),
                _ => panic!("> should be plused with number and number"),
            },
            _ => panic!("> should be plused with number and number"),
        },
        InfixOperator::GRATER_EQUAL => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value >= right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) >= right.value,
                }),
                _ => panic!(">= should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value >= (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value >= right.value,
                }),
                _ => panic!(">= should be plused with number and number"),
            },
            _ => panic!(">= should be plused with number and number"),
        },
        InfixOperator::LESS => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value < right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) < right.value,
                }),
                _ => panic!("< should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value < (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value < right.value,
                }),
                _ => panic!("< should be plused with number and number"),
            },
            _ => panic!("< should be plused with number and number"),
        },
        InfixOperator::LESS_EQUAL => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value <= right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) <= right.value,
                }),
                _ => panic!("<= s>hould be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value <= (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value <= right.value,
                }),
                _ => panic!("<= s>hould be plused with number and number"),
            },
            _ => panic!("<= s>hould be plused with number and number"),
        },
        InfixOperator::EQUAL => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value == right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) == right.value,
                }),
                _ => panic!("== should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value == (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value == right.value,
                }),
                _ => panic!("== should be plused with number and number"),
            },
            _ => panic!("== should be plused with number and number"),
        },
        InfixOperator::NOT_EQUAL => match left {
            Object::Integer(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value != right.value,
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: (left.value as f32) != right.value,
                }),
                _ => panic!("!= should be plused with number and number"),
            },
            Object::Float(left) => match right {
                Object::Integer(right) => Object::Bool(Bool {
                    value: left.value != (right.value as f32),
                }),
                Object::Float(right) => Object::Bool(Bool {
                    value: left.value != right.value,
                }),
                _ => panic!("!= should be plused with number and number"),
            },
            _ => panic!("!= should be plused with number and number"),
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn evaluate_eval_int_works() {
        let src: String = String::from("5;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "5".to_string());
    }

    #[test]
    fn evaluate_eval_bool_works() {
        let src: String = String::from("false;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "false".to_string());
    }

    #[test]
    fn evaluate_prefix_expression_minus_int() {
        let src: String = String::from("-5;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "-5".to_string());
    }

    #[test]
    fn evaluate_prefix_expression_minus_float() {
        let src: String = String::from("-1.25;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "-1.25".to_string());
    }

    #[test]
    fn evaluate_infix_expression_int() {
        let src: String = String::from("5 + 6;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "11".to_string());

        let src: String = String::from("5 * 6;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "30".to_string());

        let src: String = String::from("5 - 6;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "-1".to_string());

        let src: String = String::from("6 / 2;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "3".to_string());
    }

    #[test]
    fn evaluate_infix_expression_float() {
        let src: String = String::from("5 * 6.2;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "31".to_string());

        let src: String = String::from("5 + 6.2;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "11.2".to_string());

        let src: String = String::from("5 == 5.0;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "true".to_string());

        let src: String = String::from("5 / 2;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "2.5".to_string());

        let src: String = String::from(
            "fn add(left, right) {
            left + right;
        };
        
        add(2, 3);",
        );
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "5".to_string());
    }

    #[test]
    fn evaluate_eval_variable_declaration() {
        let src: String = String::from("let five = 5.5;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "5.5".to_string());
        let src: String = String::from("five;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "5.5".to_string());
    }

    #[test]
    fn evaluate_eval_user_defined_function() {
        let src: String = String::from(
            "let five = 5;
let ten = 10;
fn add(left, right) {
    left + right;
};",
        );
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "[object object]".to_string());
        let src: String = String::from("add(five, ten);");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "15".to_string());
        let src: String = String::from("add(3, 4);");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "7".to_string());
        let src: String = String::from("add(3 * 4, ten);");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);

        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "22".to_string());
    }

    #[test]
    fn evaluate_return_statement_works() {
        let src: String = String::from("fn add(l, r) {
            return l + r;
            l - r;
        };
        
        add(3, 2);");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let mut break_flag = false;
        let result = evaluate(node, &mut environment, &mut break_flag);
        assert_eq!(result.inspect(), "5".to_string());
    }
}
