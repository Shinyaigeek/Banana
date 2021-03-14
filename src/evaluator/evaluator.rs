use crate::evaluator::object::object::{Bool, Integer, Null, Object};
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{
    BlockStatement, Expression, IfStatement, InfixOperator, Literal, LiteralType, Node, Parser,
    PrefixOperator, Statement, StatementType,
};
use crate::parser::tokenizer::tokenizer::Tokens;

pub fn evaluate(node: Node) -> Object {
    match node {
        Node::Program(program) => evaluate_statements(program.body),
        Node::Statement(statement) => match statement.statement {
            StatementType::Expression(expression) => handle_expression(Box::new(expression)),
            StatementType::IfStatement(if_statement) => handle_if_statement(if_statement),
            _ => panic!(""),
        },
        Node::Expression(expression) => handle_expression(Box::new(expression)),
        _ => panic!(""),
    }
}

pub fn evaluate_statements(statements: Vec<Statement>) -> Object {
    let mut result: Option<Object> = None;
    for statement in statements {
        result = Some(evaluate(statement_to_node(statement)));
    }
    match result {
        Some(result) => result,
        None => panic!(""),
    }
}

pub fn statement_to_node(statement: Statement) -> Node {
    Node::Statement(statement)
}

pub fn handle_if_statement(if_statement: IfStatement) -> Object {
    let test = handle_expression(if_statement.test);
    let test = match test {
        Object::Bool(bool) => bool,
        _ => panic!("if's test should be boolean"),
    };
    if test.value {
        evaluate_statements(if_statement.consequents)
    } else {
        let alternate = if_statement.alternate;
        match *alternate {
            Some(alternate) => match alternate {
                StatementType::IfStatement(if_statement) => handle_if_statement(if_statement),
                // * else
                StatementType::BlockStatement(block_statement) => {
                    handle_block_statement(block_statement)
                }
                _ => panic!("if statements' alternate should be if_statement or block statement"),
            },
            // * else
            None => Object::Null(Null {}),
        }
    }
}

pub fn handle_block_statement(block_statement: BlockStatement) -> Object {
    evaluate_statements(block_statement.body)
}

pub fn handle_expression(expression: Box<Expression>) -> Object {
    match *expression {
        Expression::Literal(literal) => match literal {
            Literal::Integer(int) => {
                let int: i32 = int.value.parse().unwrap();
                Object::Integer(Integer { value: int })
            }

            Literal::Boolean(boolean) => {
                let boolean: bool = boolean.value.parse().unwrap();
                Object::Bool(Bool { value: boolean })
            }
            _ => panic!(""),
        },
        Expression::PrefixExpression(prefix_expression) => {
            let obj = handle_expression(prefix_expression.right);
            match prefix_expression.operator {
                PrefixOperator::MINUS => handle_prefix_literal(obj, PrefixOperator::MINUS),
                PrefixOperator::EXCLAMATION => {
                    handle_prefix_literal(obj, PrefixOperator::EXCLAMATION)
                }
                _ => panic!("prefix operator should be - or !"),
            }
        }
        Expression::InfixExpression(infix_expression) => {
            let left = handle_expression(infix_expression.left);
            let right = handle_expression(infix_expression.right);
            handle_infix_expression(left, right, infix_expression.operator)
        }
        _ => panic!(""),
    }
}

pub fn handle_prefix_literal(obj: Object, prefix: PrefixOperator) -> Object {
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
        }
    }
}

pub fn handle_infix_expression(left: Object, right: Object, operator: InfixOperator) -> Object {
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
        let result = evaluate(node);
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
        let result = evaluate(node);
        assert_eq!(result.inspect(), "false".to_string());
    }
}
