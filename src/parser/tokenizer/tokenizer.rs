use crate::parser::lexer::lexer::Lexer;

pub const EOF: u8 = 0;
pub const ASSIGN: u8 = b'=';
pub const PLUS: u8 = b'+';
pub const COMMA: u8 = b',';
pub const SEMICOLON: u8 = b';';
pub const LPAREN: u8 = b'(';
pub const RPAREN: u8 = b')';
pub const LBRACE: u8 = b'{';
pub const RBRACE: u8 = b'}';

pub const LET: &str = "LET";
pub const FUNCTION: &str = "fn";
pub const IDENTIFIER: &str = "IDENTIFIER";
pub const INT: &str = "INT";
pub const RETURN: &str = "return";

pub const ILLEGAL: &str = "ILLEGAL";

#[derive(Debug, PartialEq)]
pub enum TokenType {
    EOF,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LET,
    FUNCTION,
    IDENTIFIER,
    INT,
    RETURN,
    ILLEGAL,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: TokenType,
    value: String,
}

impl Token {
    pub fn new(token_type: TokenType, bytes: Vec<u8>) -> Self {
        Token {
            token_type,
            value: String::from_utf8(bytes).unwrap(),
        }
    }

    // ! THIS IS A METHOD FOR TEST, DON'T USE THIS METHOD
    pub fn __raw_new_(token_type: TokenType, value: String) -> Self {
        Token { token_type, value }
    }
}

#[derive(Debug)]
pub struct Tokens {
    // TODO is this ok?
    tokens: Vec<Token>,
    lexer: Lexer,
}

impl Tokens {
    pub fn new(lexer: Lexer) -> Self {
        let mut token = Tokens {
            tokens: Vec::new(),
            lexer,
        };

        token.tokenize();

        token
    }

    fn tokenize(&mut self) {
        loop {
            let ch = self.lexer.peek();

            let token = match ch {
                ASSIGN => Token::new(TokenType::ASSIGN, vec![self.lexer.read_char()]),
                PLUS => Token::new(TokenType::PLUS, vec![self.lexer.read_char()]),
                COMMA => Token::new(TokenType::COMMA, vec![self.lexer.read_char()]),
                SEMICOLON => Token::new(TokenType::SEMICOLON, vec![self.lexer.read_char()]),
                LPAREN => Token::new(TokenType::LPAREN, vec![self.lexer.read_char()]),
                RPAREN => Token::new(TokenType::RPAREN, vec![self.lexer.read_char()]),
                LBRACE => Token::new(TokenType::LBRACE, vec![self.lexer.read_char()]),
                RBRACE => Token::new(TokenType::RBRACE, vec![self.lexer.read_char()]),
                0 => Token::new(TokenType::EOF, vec![self.lexer.read_char()]),
                _ => Token::new(TokenType::ILLEGAL, vec![self.lexer.read_char()]),
            };

            if token.token_type == TokenType::EOF {
                self.tokens.push(Token::new(TokenType::EOF, vec![ch]));
                break;
            } else {
                self.tokens.push(token);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn tokenize_works() {
        let mut lexer = Lexer::new(&String::from("(){}+"));

        let tokens = Tokens::new(lexer);

        assert_eq!(
            tokens.tokens,
            vec![
                Token::new(TokenType::LPAREN, vec![b'(']),
                Token::new(TokenType::RPAREN, vec![b')']),
                Token::new(TokenType::LBRACE, vec![b'{']),
                Token::new(TokenType::RBRACE, vec![b'}']),
                Token::new(TokenType::PLUS, vec![b'+']),
                Token::new(TokenType::EOF, vec![0]),
            ]
        );

        let mut lexer = Lexer::new(&String::from(
            "let five = 5;
        let ten = 10;
        
        fn add(left, right) {
            return five + ten;
        }
        
        add(five, ten",
        ));
        let tokens = Tokens::new(lexer);
        // assert_eq!(
        //     tokens.tokens,
        //     vec![
        //         Token::__raw_new_(TokenType::LET, String::from("let")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
        //         Token::__raw_new_(TokenType::INT, String::from("5")),
        //         Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
        //         Token::__raw_new_(TokenType::LET, String::from("let")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
        //         Token::__raw_new_(TokenType::INT, String::from("10")),
        //         Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
        //         Token::__raw_new_(TokenType::FUNCTION, String::from("fn")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("add")),
        //         Token::__raw_new_(TokenType::LPAREN, String::from("(")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("left")),
        //         Token::__raw_new_(TokenType::COMMA, String::from(",")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("right")),
        //         Token::__raw_new_(TokenType::RPAREN, String::from(")")),
        //         Token::__raw_new_(TokenType::LBRACE, String::from("{")),
        //         Token::__raw_new_(TokenType::RETURN, String::from("return")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
        //         Token::__raw_new_(TokenType::PLUS, String::from("+")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
        //         Token::__raw_new_(TokenType::RBRACE, String::from("}")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("add")),
        //         Token::__raw_new_(TokenType::LPAREN, String::from("(")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("left")),
        //         Token::__raw_new_(TokenType::COMMA, String::from(",")),
        //         Token::__raw_new_(TokenType::IDENTIFIER, String::from("right")),
        //         Token::__raw_new_(TokenType::RPAREN, String::from(")")),
        //     ]
        // )
    }
}
