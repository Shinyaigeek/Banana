mod parser;
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::Parser;
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

fn main() {
    let src = "-1 + 3 * -6 + 2;".to_string();
    let mut lexer = Lexer::new(&src);
    let mut tokens = Tokens::new(lexer);
    let mut parser = Parser::new(tokens);
    parser.parse();
    println!("{:?}", parser.program);
}
