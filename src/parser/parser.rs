use crate::parser::lexer::lexer::Lexer;
use crate::parser::tokenizer::tokenizer::{
    Token, TokenType, Tokens, ASTERISK, EQUAL, EXCLAMATION, GRATER, GRATER_EQUAL, LESS, LESS_EQUAL,
    MINUS, NOT_EQUAL, PLUS, SLASH,
};

const LOWEST: u8 = 0;
const EQUALS: u8 = 1; // ==
const LESSGREATER: u8 = 2; // > or <
const SUM: u8 = 3; // +
const PRODUCT: u8 = 4; // *
const PREFIX: u8 = 5; // !X or -X
const CALL: u8 = 6; // function(x)

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Precedence {
    LOWEST,
    EQUALS,
    LESSGREATER,
    SUM,
    PRODUCT,
    PREFIX,
    CALL,
}

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
    Expression(Expression),
    IfStatement(IfStatement),
    BlockStatement(BlockStatement)
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    test: Box<Expression>,
    alternate: Box<Option<StatementType>>,
    consequents: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    body: Box<Vec<Statement>>
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
    arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Literal(Literal),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    Identifier(Identifier),
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    operator: PrefixOperator,
    right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    operator: InfixOperator,
    right: Box<Expression>,
    left: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    EXCLAMATION,
    MINUS,
}

#[derive(Debug, PartialEq)]
pub enum InfixOperator {
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    GRATER,
    GRATER_EQUAL,
    LESS,
    LESS_EQUAL,
    EQUAL,
    NOT_EQUAL,
}

#[derive(Debug, PartialEq)]
pub struct Literal {
    value: String,
    literal_type: LiteralType,
}

#[derive(Debug, PartialEq)]
pub enum LiteralType {
    INT,
    BOOLEAN,
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
        parser
    }

    pub fn parse(&mut self) {
        let statements = self.handle_statements();
        self.program.body = statements;
        println!("parse execution done 🎉");
    }

    fn handle_statements(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];
        loop {
            // TODO should fix
            let token = if Parser::is_semicolon(self.tokens.cur_token()) {
                self.tokens.read_token()
            } else {
                self.tokens.cur_token()
            };
            let statement = if Parser::is_variable_declaration(token) {
                self.handle_variable_declaration()
            } else if Parser::is_return_statement(token) {
                self.handle_return_statement()
            } else if token.token_type == TokenType::EOF || token.token_type == TokenType::RBRACE {
                break;
            } else if Parser::is_if_block(token) {
                self.handle_if_statement()
            } else {
                self.handle_expression_statement()
            };

            let statement = Statement { statement };
            statements.push(statement);

            if self.tokens.peek_token().token_type == TokenType::SEMICOLON {
                self.tokens.read_token();
            }

            if self.tokens.peek_token().token_type == TokenType::EOF
                || self.tokens.peek_token().token_type == TokenType::RBRACE
            {
                break;
            }
        }

        statements
    }

    fn is_return_statement(token: &Token) -> bool {
        token.token_type == TokenType::RETURN
    }

    fn is_variable_declaration(token: &Token) -> bool {
        token.token_type == TokenType::LET
    }

    fn is_if_block(token: &Token) -> bool {
        token.token_type == TokenType::IF
    }

    fn is_assign(token: &Token) -> bool {
        token.token_type == TokenType::ASSIGN
    }

    fn is_expression(token: &Token) -> bool {
        Parser::is_literal(&token)
    }

    fn is_literal(token: &Token) -> bool {
        Parser::is_number(&token) || Parser::is_boolean(&token)
    }

    fn is_number(token: &Token) -> bool {
        token.token_type == TokenType::INT
    }

    fn is_boolean(token: &Token) -> bool {
        token.token_type == TokenType::TRUE || token.token_type == TokenType::FALSE
    }

    fn is_identifier(token: &Token) -> bool {
        token.token_type == TokenType::IDENTIFIER
    }

    fn is_sum_operator(token: &Token) -> bool {
        token.token_type == TokenType::PLUS || token.token_type == TokenType::MINUS
    }

    fn is_product_operator(token: &Token) -> bool {
        token.token_type == TokenType::ASTERISK || token.token_type == TokenType::SLASH
    }

    fn is_equal_operator(token: &Token) -> bool {
        token.token_type == TokenType::EQUAL || token.token_type == TokenType::NOT_EQUAL
    }

    fn is_greater_less_operator(token: &Token) -> bool {
        token.token_type == TokenType::GRATER
            || token.token_type == TokenType::LESS
            || token.token_type == TokenType::GRATER_EQUAL
            || token.token_type == TokenType::LESS_EQUAL
    }

    fn is_prefix_operator(token: &Token) -> bool {
        token.token_type == TokenType::EXCLAMATION
            || token.token_type == TokenType::MINUS
            || token.token_type == TokenType::LPAREN
    }

    fn is_semicolon(token: &Token) -> bool {
        token.token_type == TokenType::SEMICOLON
    }

    fn is_left_precedencer(left: Precedence, right: Precedence) -> bool {
        left as u8 > right as u8
    }

    fn parse_identifier(token: &Token) -> Expression {
        Expression::Identifier(Identifier {
            value: token.value.clone(),
        })
    }

    fn parse_literal(token: &Token) -> Expression {
        if Parser::is_number(token) {
            Parser::parse_int(token)
        } else if Parser::is_boolean(token) {
            Parser::parse_boolean(token)
        } else {
            panic!("parse_literal should get only int");
        }
    }

    fn parse_int(token: &Token) -> Expression {
        Expression::Literal(Literal {
            value: token.value.clone(),
            literal_type: LiteralType::INT,
        })
    }

    fn parse_boolean(token: &Token) -> Expression {
        Expression::Literal(Literal {
            value: token.value.clone(),
            literal_type: LiteralType::BOOLEAN,
        })
    }

    fn parse_prefix_operator(token: &Token) -> PrefixOperator {
        if token.token_type == TokenType::EXCLAMATION {
            PrefixOperator::EXCLAMATION
        } else if token.token_type == TokenType::MINUS {
            PrefixOperator::MINUS
        } else {
            panic!("parse_prefix_operator should get exclamation or minus");
        }
    }

    fn parse_infix_operator(token: &Token) -> InfixOperator {
        if token.token_type == TokenType::PLUS {
            InfixOperator::PLUS
        } else if token.token_type == TokenType::MINUS {
            InfixOperator::MINUS
        } else if token.token_type == TokenType::ASTERISK {
            InfixOperator::ASTERISK
        } else if token.token_type == TokenType::SLASH {
            InfixOperator::SLASH
        } else if token.token_type == TokenType::GRATER {
            InfixOperator::GRATER
        } else if token.token_type == TokenType::GRATER_EQUAL {
            InfixOperator::GRATER_EQUAL
        } else if token.token_type == TokenType::LESS {
            InfixOperator::LESS
        } else if token.token_type == TokenType::LESS_EQUAL {
            InfixOperator::LESS_EQUAL
        } else if token.token_type == TokenType::EQUAL {
            InfixOperator::EQUAL
        } else if token.token_type == TokenType::NOT_EQUAL {
            InfixOperator::NOT_EQUAL
        } else {
            panic!("parse_infix_operator should get exclamation or minus");
        }
    }

    fn peek_precedence(&self) -> Precedence {
        let token = self.tokens.peek_token();

        if Parser::is_sum_operator(&token) {
            Precedence::SUM
        } else if Parser::is_product_operator(&token) {
            Precedence::PRODUCT
        } else if Parser::is_equal_operator(&token) {
            Precedence::EQUALS
        } else if Parser::is_greater_less_operator(&token) {
            Precedence::LESSGREATER
        } else {
            Precedence::LOWEST
        }
    }

    fn cur_precedence(&self) -> Precedence {
        let token = self.tokens.cur_token();

        if Parser::is_sum_operator(&token) {
            Precedence::SUM
        } else if Parser::is_product_operator(&token) {
            Precedence::PRODUCT
        } else if Parser::is_equal_operator(&token) {
            Precedence::EQUALS
        } else if Parser::is_greater_less_operator(&token) {
            Precedence::LESSGREATER
        } else {
            Precedence::LOWEST
        }
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let token = if Parser::is_semicolon(self.tokens.cur_token()) {
            self.tokens.read_token()
        } else {
            self.tokens.cur_token()
        };

        let mut left_expression = if Parser::is_literal(&token) {
            Parser::parse_literal(&token)
        } else if Parser::is_identifier(&token) {
            Parser::parse_identifier(&token)
        } else if Parser::is_prefix_operator(&token) {
            self.parse_prefix_expression()
        } else {
            panic!("prefix should be literal or identifier or prefix")
        };

        // TODO
        while !Parser::is_semicolon(self.tokens.peek_token())
            && !Parser::is_left_precedencer(precedence, self.peek_precedence())
            && self.tokens.peek_token().token_type != TokenType::EOF
            && self.tokens.peek_token().token_type != TokenType::LBRACE
        {
            let token = self.tokens.read_token();

            left_expression = if Parser::is_sum_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_product_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_equal_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else if Parser::is_greater_less_operator(&token) {
                self.parse_infix_expression(left_expression)
            } else {
                left_expression
            };
        }

        left_expression
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let token = self.tokens.cur_token();
        if token.token_type == TokenType::LPAREN {
            self.tokens.read_token();
            let expression = self.parse_expression(Precedence::LOWEST);
            if self.tokens.peek_token().token_type == TokenType::RPAREN {
                panic!(") should be next to (");
            }

            return expression;
        }
        let operator = Parser::parse_prefix_operator(&token);
        self.tokens.read_token();
        let expression = PrefixExpression {
            operator,
            right: Box::new(self.parse_expression(Precedence::PREFIX)),
        };

        Expression::PrefixExpression(expression)
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.tokens.cur_token();
        let operator = Parser::parse_infix_operator(token);

        let precedence = self.cur_precedence();

        self.tokens.read_token();

        let expression = InfixExpression {
            operator,
            right: Box::new(self.parse_expression(precedence)),
            left: Box::new(left),
        };

        Expression::InfixExpression(expression)
    }

    fn handle_expression_statement(&mut self) -> StatementType {
        let expression = self.parse_expression(Precedence::LOWEST);

        StatementType::Expression(expression)
    }

    fn handle_if_statement(&mut self) -> StatementType {
        let l_paren = if self.tokens.peek_token().token_type == TokenType::IF {
            self.tokens.read_token();
            self.tokens.read_token()
        } else {
            self.tokens.read_token()
        };

        let l_paren = self.tokens.peek_token();

        if l_paren.token_type == TokenType::LPAREN {
            panic!("if statement's test should braced ()");
        }
        let test = self.parse_expression(Precedence::LOWEST);

        if self.tokens.read_token().token_type == TokenType::RPAREN {
            panic!(") should be next to (");
        }

        let l_brace = self.tokens.cur_token();

        if l_brace.token_type != TokenType::LBRACE {
            panic!("if statement's consequents should braced {}");
        }

        self.tokens.read_token();

        let consequents = self.handle_statements();

        let r_brace = if self.tokens.cur_token().token_type == TokenType::SEMICOLON {
            self.tokens.read_token()
        } else {
            self.tokens.cur_token()
        };

        if r_brace.token_type != TokenType::RBRACE {
            panic!("if statements' consecuents should end with }")
        }

        if self.tokens.peek_token().token_type == TokenType::ELSE {
            self.tokens.read_token();
            let alternate = self.handle_if_statement();

            return StatementType::IfStatement(IfStatement {
                test: Box::new(test),
                consequents,
                alternate: Box::new(Some(alternate)),
            });
        }

        StatementType::IfStatement(IfStatement {
            test: Box::new(test),
            consequents,
            alternate: Box::new(None),
        })
    }

    fn handle_return_statement(&mut self) -> StatementType {
        let return_token = self.tokens.cur_token();
        if return_token.token_type != TokenType::RETURN {
            panic!("handle_return_statement can only handle return");
        }

        let mut arguments: Vec<Expression> = vec![];

        loop {
            let token = self.tokens.read_token();

            if token.token_type == TokenType::SEMICOLON {
                let statement = ReturnStatement { arguments };
                return StatementType::ReturnStatement(statement);
            } else {
                let argument = self.parse_expression(Precedence::LOWEST);
                arguments.push(argument);
            }
        }
    }

    fn handle_variable_declaration(&mut self) -> StatementType {
        // TODO should fix
        let declaration_token = self.tokens.cur_token();
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

            let initializer = self.parse_expression(Precedence::LOWEST);

            let statement = VariableDeclaration {
                // TODO support const
                kind: String::from("let"),
                identifier: identifier,
                mutation: is_mutate,
                init: initializer,
            };

            return StatementType::VariableDeclaration(statement);

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
        let ten = 2 * 8;
        let i = (1 + 2) * 8;
        let t = true;
        let f = !true;",
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
                        init: Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal {
                                value: "8".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                            left: Box::new(Expression::Literal(Literal {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("i"),
                        },
                        mutation: false,
                        init: Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal {
                                value: "8".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                            left: Box::new(Expression::InfixExpression(InfixExpression {
                                operator: InfixOperator::PLUS,
                                right: Box::new(Expression::Literal(Literal {
                                    value: "2".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                                left: Box::new(Expression::Literal(Literal {
                                    value: "1".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                            })),
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("t"),
                        },
                        mutation: false,
                        init: Expression::Literal(Literal {
                            value: "true".to_string(),
                            literal_type: LiteralType::BOOLEAN,
                        }),
                    }),
                },
                Statement {
                    statement: StatementType::VariableDeclaration(VariableDeclaration {
                        kind: String::from("let"),
                        identifier: Identifier {
                            value: String::from("f"),
                        },
                        mutation: false,
                        init: Expression::PrefixExpression(PrefixExpression {
                            operator: PrefixOperator::EXCLAMATION,
                            right: Box::new(Expression::Literal(Literal {
                                value: "true".to_string(),
                                literal_type: LiteralType::BOOLEAN,
                            })),
                        }),
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);

        let mut lexer = Lexer::new(&String::from(
            "let mut five = 5;
        return 5;
        return 2 * 8;",
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
                        arguments: vec![Expression::Literal(Literal {
                            value: String::from("5"),
                            literal_type: LiteralType::INT,
                        })],
                    }),
                },
                Statement {
                    statement: StatementType::ReturnStatement(ReturnStatement {
                        arguments: vec![Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::ASTERISK,
                            right: Box::new(Expression::Literal(Literal {
                                value: "8".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                            left: Box::new(Expression::Literal(Literal {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                        })],
                    }),
                },
            ],
        };

        assert_eq!(parser.program, expected);

        let mut lexer = Lexer::new(&String::from("1 + 3 * 6 + 2;"));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![Statement {
                statement: StatementType::Expression(Expression::InfixExpression(
                    InfixExpression {
                        operator: InfixOperator::PLUS,
                        right: Box::new(Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::PLUS,
                            right: Box::new(Expression::Literal(Literal {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                            left: Box::new(Expression::InfixExpression(InfixExpression {
                                operator: InfixOperator::ASTERISK,
                                right: Box::new(Expression::Literal(Literal {
                                    value: "6".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                                left: Box::new(Expression::Literal(Literal {
                                    value: "3".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                            })),
                        })),
                        left: Box::new(Expression::Literal(Literal {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        })),
                    },
                )),
            }],
        };

        assert_eq!(parser.program, expected);

        let mut lexer = Lexer::new(&String::from("-1 + 3 * -6 + 2;"));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![Statement {
                statement: StatementType::Expression(Expression::InfixExpression(
                    InfixExpression {
                        operator: InfixOperator::PLUS,
                        right: Box::new(Expression::InfixExpression(InfixExpression {
                            operator: InfixOperator::PLUS,
                            right: Box::new(Expression::Literal(Literal {
                                value: "2".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                            left: Box::new(Expression::InfixExpression(InfixExpression {
                                operator: InfixOperator::ASTERISK,
                                right: Box::new(Expression::PrefixExpression(PrefixExpression {
                                    operator: PrefixOperator::MINUS,
                                    right: Box::new(Expression::Literal(Literal {
                                        value: "6".to_string(),
                                        literal_type: LiteralType::INT,
                                    })),
                                })),
                                left: Box::new(Expression::Literal(Literal {
                                    value: "3".to_string(),
                                    literal_type: LiteralType::INT,
                                })),
                            })),
                        })),
                        left: Box::new(Expression::PrefixExpression(PrefixExpression {
                            operator: PrefixOperator::MINUS,
                            right: Box::new(Expression::Literal(Literal {
                                value: "1".to_string(),
                                literal_type: LiteralType::INT,
                            })),
                        })),
                    },
                )),
            }],
        };

        assert_eq!(parser.program, expected);

        let mut lexer = Lexer::new(&String::from(
            "if (1 == 1) {
            let hoge = 33;
        } else if (true) {
            let fuga = 909;
        };",
        ));
        let mut tokens = Tokens::new(lexer);
        let mut parser = Parser::new(tokens);
        parser.parse();
        let expected = Program {
            body: vec![Statement {
                statement: StatementType::IfStatement(IfStatement {
                    test: Box::new(Expression::InfixExpression(InfixExpression {
                        operator: InfixOperator::EQUAL,
                        right: Box::new(Expression::Literal(Literal {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        })),
                        left: Box::new(Expression::Literal(Literal {
                            value: "1".to_string(),
                            literal_type: LiteralType::INT,
                        })),
                    })),
                    alternate: Box::new(Some(StatementType::IfStatement(IfStatement {
                        test: Box::new(Expression::Literal(Literal {
                            value: "true".to_string(),
                            literal_type: LiteralType::BOOLEAN,
                        })),
                        alternate: Box::new(None),
                        consequents: vec![Statement {
                            statement: StatementType::VariableDeclaration(VariableDeclaration {
                                kind: "let".to_string(),
                                identifier: Identifier {
                                    value: "fuga".to_string(),
                                },
                                mutation: false,
                                init: Expression::Literal(Literal {
                                    value: "909".to_string(),
                                    literal_type: LiteralType::INT,
                                }),
                            }),
                        }],
                    }))),
                    consequents: vec![Statement {
                        statement: StatementType::VariableDeclaration(VariableDeclaration {
                            kind: "let".to_string(),
                            identifier: Identifier {
                                value: "hoge".to_string(),
                            },
                            mutation: false,
                            init: Expression::Literal(Literal {
                                value: "33".to_string(),
                                literal_type: LiteralType::INT,
                            }),
                        }),
                    }],
                }),
            }],
        };

        assert_eq!(parser.program, expected);
    }
}
