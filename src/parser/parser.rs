use crate::parser::lexer::lexer::Lexer;
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

#[derive(Debug, PartialEq)]
pub struct Program {
    body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Statement {
    statement: StatementType,
}

#[derive(Debug, PartialEq)]
pub enum StatementType {
    VariableDeclaration(VariableDeclaration),
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    // TODO LiteralUnionField
    kind: String,
    identifier: Token,
    mutation: bool,
    init: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    value: Token,
}

#[derive(Debug, PartialEq)]
pub struct Parser {
    tokens: Tokens,
    pub program: Program,
}

impl Parser {
    pub fn new(tokens: Tokens) -> Self {
        let mut program = Program { body: vec![] };
        let mut parser = Parser { tokens, program };
        parser.parse();
        parser
    }

    pub fn parse(&mut self) {
        loop {
            // TODO should fix
            let token = self.tokens.peek_token();

            if Parser::is_variable_declaration(token) {
                let statement = self.handle_variable_declaration();
                let statement = Statement { statement };
                self.program.body.push(statement)
            }

            if self.tokens.peek_token().token_type == TokenType::SEMICOLON {
                self.tokens.read_token();
            }

            if self.tokens.peek_token().token_type == TokenType::EOF {
                break;
            }
        }
        println!("parse execution done 🎉");
    }

    fn is_variable_declaration(token: &Token) -> bool {
        token.token_type == TokenType::LET
    }

    fn is_assign(token: &Token) -> bool {
        token.token_type == TokenType::ASSIGN
    }

    fn is_expression(token: &Token) -> bool {
        Parser::is_literal(&token)
    }

    fn is_literal(token: &Token) -> bool {
        Parser::is_number(&token)
    }

    fn is_number(token: &Token) -> bool {
        token.token_type == TokenType::INT
    }

    fn is_identifier(token: &Token) -> bool {
        token.token_type == TokenType::IDENTIFIER
    }

    fn handle_variable_declaration(&mut self) -> StatementType {
        // TODO should fix
        let declaration_token = self.tokens.read_token();
        // TODO 今の所ExpressionにはLiteral Intしか来ないようにしている
        if declaration_token.token_type == TokenType::LET {
            let token = Token::copy_token(self.tokens.read_token());
            let is_mutate = if token.token_type == TokenType::MUTATE {
                true
            } else {
                false
            };

            let identifier = if is_mutate {
                Token::copy_token(self.tokens.read_token())
            } else {
                token
            };

            if Parser::is_identifier(&identifier) == false {
                panic!("TokenType::IDENTIFIER should be next to let or let mut");
            }

            let token = Token::copy_token(self.tokens.read_token());

            // TODO ! ?
            if Parser::is_assign(&token) == false {
                panic!("TokenType::ASSIGN should be next to let identifier or let mut identifier");
            }

            let initializer = self.tokens.read_token();

            if Parser::is_expression(initializer) {
                if Parser::is_literal(initializer) {
                    let initializer = Literal {
                        value: Token::copy_token(initializer),
                    };
                    let initializer = Expression::Literal(initializer);
                    let statement = VariableDeclaration {
                        // TODO support const
                        kind: String::from("let"),
                        identifier: Token::copy_token(&identifier),
                        mutation: is_mutate,
                        init: initializer,
                    };
                    return StatementType::VariableDeclaration(statement);
                } else {
                    panic!("expression value should be literal type");
                }
            }

            panic!("VariableDeclaration's initializer should be expression");
        } else {
            panic!("handleVariableDeclaration should take LET as Token::token_type");
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_works() {
        //* variable declaration
        let mut lexer = Lexer::new(&String::from(
            "let five = 5;
        let ten = 10;",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
                        mutation: false,
                        init: Expression::Literal(Literal {
                            value: Token::__raw_new_(TokenType::INT, String::from("5")),
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
                        mutation: false,
                        init: Expression::Literal(Literal {
                            value: Token::__raw_new_(TokenType::INT, String::from("10")),
                        }),
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);
    }
}
