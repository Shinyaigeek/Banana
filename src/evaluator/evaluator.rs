use crate::evaluator::object::object::{Bool, Integer, Object};
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{
    Expression, Literal, LiteralType, Node, Parser, Statement, StatementType,
};
use crate::parser::tokenizer::tokenizer::Tokens;

pub fn evaluate(node: Node) -> Object {
    match node {
        Node::Program(program) => evaluate_statements(program.body),
        Node::Statement(statement) => match statement.statement {
            StatementType::Expression(expression) => handle_expression(expression),
            _ => panic!(""),
        },
        Node::Expression(expression) => handle_expression(expression),
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

pub fn handle_expression(expression: Expression) -> Object {
    match expression {
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
