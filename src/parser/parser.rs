use crate::parser::lexer::lexer::Lexer;
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

pub struct Program {
    body: Vec<Statement>,
}

pub struct Statement {}

pub enum StatementType {}

pub struct VariableDeclaration {
    // TODO LiteralUnionField
    kind: String,
    identifier: Token,
    mutation: Boolean,
    init: Expression,
}

pub enum Expression {
    Literal,
}

pub struct Literal {
    raw: String,
    value: Token,
}

pub struct Parser {
    tokens: Tokens,
    program: Program,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        let program = Program { body: vec![] };
        Parser { tokens, program }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
}
