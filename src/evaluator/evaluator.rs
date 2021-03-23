use crate::evaluator::object::object::{Bool, Function, Integer, Null, Object, Float};
use crate::evaluator::variable::variable::{Environment, VariableValue};
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{
    BlockStatement, Expression, IfStatement, InfixOperator, Literal, LiteralType, Node, Parser,
    PrefixOperator, Statement, StatementType,
};
use crate::parser::tokenizer::tokenizer::Tokens;

pub fn evaluate(node: Node, environment: &mut Environment) -> Object {
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
                        res.push(argument.value);
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
            _ => panic!("{:?}", statement),
        },
        Node::Expression(expression) => handle_expression(Box::new(expression), environment),
        _ => panic!(""),
    }
}

pub fn evaluate_statements(statements: Vec<Statement>, environment: &mut Environment) -> Object {
    let mut result: Option<Object> = None;
    for statement in statements {
        result = Some(evaluate(statement_to_node(statement), environment));
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
            let mut environment_with_args = environment.extend();
            for idx in 0..(call.arguments.len()) {
                // TODO valueがidentifier限定になってる(ASTレベルで)
                let value = environment.get(call_expression.arguments[idx].value.clone());
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
        InfixOperator::PLUS => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("+ should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("+ should be plused with integer and integer"),
            };

            Object::Integer(Integer {
                value: left.value + right.value,
            })
        }
        InfixOperator::MINUS => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("- should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("- should be plused with integer and integer"),
            };

            Object::Integer(Integer {
                value: left.value - right.value,
            })
        }
        InfixOperator::ASTERISK => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("* should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("* should be plused with integer and integer"),
            };

            Object::Integer(Integer {
                value: left.value * right.value,
            })
        }
        InfixOperator::SLASH => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("/ should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("/ should be plused with integer and integer"),
            };

            Object::Integer(Integer {
                value: left.value / right.value,
            })
        }
        InfixOperator::GRATER => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("> should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("> should be plused with integer and integer"),
            };

            Object::Bool(Bool {
                value: left.value > right.value,
            })
        }
        InfixOperator::GRATER_EQUAL => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!(">= should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!(">= should be plused with integer and integer"),
            };

            Object::Bool(Bool {
                value: left.value >= right.value,
            })
        }
        InfixOperator::LESS => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("< should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("< should be plused with integer and integer"),
            };

            Object::Bool(Bool {
                value: left.value < right.value,
            })
        }
        InfixOperator::LESS_EQUAL => {
            let left = match left {
                Object::Integer(int) => int,
                _ => panic!("<= should be plused with integer and integer"),
            };

            let right = match right {
                Object::Integer(int) => int,
                _ => panic!("<= should be plused with integer and integer"),
            };

            Object::Bool(Bool {
                value: left.value <= right.value,
            })
        }
        InfixOperator::EQUAL => match left {
            Object::Integer(left) => {
                let right = match right {
                    Object::Integer(int) => int,
                    _ => panic!("== should be plused with integer and integer"),
                };

                Object::Bool(Bool {
                    value: left.value == right.value,
                })
            }
            Object::Bool(left) => {
                let right = match right {
                    Object::Bool(boolean) => boolean,
                    _ => panic!("== should be plused with boolean and boolean"),
                };

                Object::Bool(Bool {
                    value: left.value == right.value,
                })
            }
            _ => panic!("== should be plused with integer or boolean"),
        },
        InfixOperator::NOT_EQUAL => match left {
            Object::Integer(left) => {
                let right = match right {
                    Object::Integer(int) => int,
                    _ => panic!("!= should be plused with integer and integer"),
                };

                Object::Bool(Bool {
                    value: left.value != right.value,
                })
            }
            Object::Bool(left) => {
                let right = match right {
                    Object::Bool(boolean) => boolean,
                    _ => panic!("!= should be plused with boolean and boolean"),
                };

                Object::Bool(Bool {
                    value: left.value != right.value,
                })
            }
            _ => panic!("!= should be plused with integer or boolean"),
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
        let result = evaluate(node, &mut environment);
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
        let result = evaluate(node, &mut environment);
        assert_eq!(result.inspect(), "false".to_string());
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
        let result = evaluate(node, &mut environment);
        assert_eq!(result.inspect(), "5.5".to_string());
        let src: String = String::from("five;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let result = evaluate(node, &mut environment);
        assert_eq!(result.inspect(), "5.5".to_string());
    }

    #[test]
    fn evaluate_eval_user_defined_function() {
        let src: String = String::from("let five = 5;
let ten = 10;
fn add(left, right) {
    left + right;
};");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let mut environment = Environment::new();
        let result = evaluate(node, &mut environment);
        assert_eq!(result.inspect(), "[object object]".to_string());
        let src: String = String::from("add(five, ten);");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let result = evaluate(node, &mut environment);
        assert_eq!(result.inspect(), "15".to_string());
    }
}
