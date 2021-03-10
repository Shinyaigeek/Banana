mod evaluator;
mod loop_input;
mod parser;
use crate::evaluator::evaluator::evaluate;
use crate::loop_input::loop_input::loop_input;
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{Node, Parser};
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

fn main() {
    loop_input(|src| {
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let node = Node::Program(parser.program);
        let result = evaluate(node);
        println!("{:?}", result);
    });
}
