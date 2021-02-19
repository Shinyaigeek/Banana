use crate::parser::lexer::lexer::Lexer;
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

pub struct Parser {
    tokens: Tokens,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        Parser { tokens }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
