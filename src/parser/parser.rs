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
    ReturnStatement(ReturnStatement),
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    value: String,
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    // TODO LiteralUnionField
    kind: String,
    identifier: Identifier,
    mutation: bool,
    init: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    arguments: Vec<Literal>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    value: String,
    literal_type: LiteralType,
}

#[derive(Debug, PartialEq)]
pub enum LiteralType {
    INT,
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
            } else if Parser::is_return_statement(token) {
                let statement = self.handle_return_statement();
                let statement = Statement { statement };
                self.program.body.push(statement);
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

    fn is_return_statement(token: &Token) -> bool {
        token.token_type == TokenType::RETURN
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

    fn handle_return_statement(&mut self) -> StatementType {
        let return_token = self.tokens.read_token();
        if return_token.token_type != TokenType::RETURN {
            panic!("handle_return_statement can only handle return");
        }

        let mut arguments: Vec<Literal> = vec![];

        loop {
            let token = self.tokens.read_token();

            if token.token_type == TokenType::SEMICOLON {
                let statement = ReturnStatement { arguments };
                return StatementType::ReturnStatement(statement);
            } else if Parser::is_literal(&token) {
                if Parser::is_number(&token) {
                    let argument = Literal {
                        value: token.value.clone(),
                        literal_type: LiteralType::INT,
                    };
                    arguments.push(argument);
                } else {
                    panic!("literal is should be number")
                }
            } else {
                panic!("return statement's argument should be literal");
            }
        }
    }

    fn handle_variable_declaration(&mut self) -> StatementType {
        // TODO should fix
        let declaration_token = self.tokens.read_token();
        // TODO 今の所ExpressionにはLiteral Intしか来ないようにしている
        if declaration_token.token_type == TokenType::LET {
            let token = self.tokens.read_token();
            let is_mutate = if token.token_type == TokenType::MUTATE {
                true
            } else {
                false
            };

            let identifier = if is_mutate {
                self.tokens.read_token()
            } else {
                token
            };

            if Parser::is_identifier(&identifier) == false {
                panic!("TokenType::IDENTIFIER should be next to let or let mut");
            }

            let identifier = Identifier {
                value: identifier.value.clone(),
            };

            let token = self.tokens.read_token();

            // TODO ! ?
            if Parser::is_assign(&token) == false {
                panic!("TokenType::ASSIGN should be next to let identifier or let mut identifier");
            }

            let token = self.tokens.read_token();

            if Parser::is_expression(token) {
                if Parser::is_literal(token) {
                    let literal_type = if Parser::is_number(&token) {
                        LiteralType::INT
                    } else {
                        panic!("literal type should be INT");
                    };
                    let initializer = Literal {
                        value: token.value.clone(),
                        literal_type,
                    };
                    let initializer = Expression::Literal(initializer);
                    let statement = VariableDeclaration {
                        // TODO support const
                        kind: String::from("let"),
                        identifier: identifier,
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
                        identifier: Identifier {
                            value: String::from("five"),
                        },
                        mutation: false,
                        init: Expression::Literal(Literal {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("ten"),
                        },
                        mutation: false,
                        init: Expression::Literal(Literal {
                            value: String::from("10"),
                            literal_type: LiteralType::INT,
                        }),
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);

        let mut lexer = Lexer::new(&String::from(
            "let mut five = 5;
        return 5;",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("five"),
                        },
                        mutation: true,
                        init: Expression::Literal(Literal {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::ReturnStatement(ReturnStatement {
                        arguments: vec![Literal {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        }],
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);
    }
}
