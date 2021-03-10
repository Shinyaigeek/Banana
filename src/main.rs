mod evaluator;
mod parser;
use crate::evaluator::evaluator::evaluate;
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{Node, Parser};
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

fn main() {
    let src = "add(r, l) + 3 + 4;".to_string();
    let mut lexer = Lexer::new(&src);
    let mut tokens = Tokens::new(lexer);
    let mut parser = Parser::new(tokens);
    parser.parse();
    let node = Node::Program(parser.program);
    evaluate(node);
    // println!("{}", parser.program.print());
}
