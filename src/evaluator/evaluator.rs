use crate::evaluator::object::object::{Bool, Integer, Object};
use crate::parser::parser::{Expression, Literal, LiteralType, Node, Statement, StatementType};

pub fn evaluate(node: Node) -> Object {
    match node {
        Node::Program(program) => evaluate_statements(program.body),
        Node::Statement(statement) => match statement.statement {
            StatementType::Expression(expression) => match expression {
                Expression::Literal(literal) => match literal.literal_type {
                    LiteralType::INT => {
                        let int: i32 = literal.value.parse().unwrap();
                        Object::Integer(Integer { value: int })
                    }
                    LiteralType::BOOLEAN => {
                        let bool: bool = literal.value.parse().unwrap();
                        Object::Bool(Bool { value: bool })
                    }
                    _ => panic!(""),
                },
                _ => panic!(""),
            },
            _ => panic!(""),
        },
        Node::Expression(expression) => match expression {
            Expression::Literal(literal) => match literal.literal_type {
                LiteralType::INT => {
                    let int: i32 = literal.value.parse().unwrap();
                    Object::Integer(Integer { value: int })
                }
                LiteralType::BOOLEAN => {
                    let bool: bool = literal.value.parse().unwrap();
                    Object::Bool(Bool { value: bool })
                }
                _ => panic!(""),
            },
            _ => panic!(""),
        },
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
