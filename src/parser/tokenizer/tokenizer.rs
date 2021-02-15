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
            let ch = self.lexer.read_char();

            let chs: Vec<u8> = vec![ch];

            let token = match ch {
                ASSIGN => Token::new(TokenType::ASSIGN, chs),
                PLUS => Token::new(TokenType::PLUS, chs),
                COMMA => Token::new(TokenType::COMMA, chs),
                SEMICOLON => Token::new(TokenType::SEMICOLON, chs),
                LPAREN => Token::new(TokenType::LPAREN, chs),
                RPAREN => Token::new(TokenType::RPAREN, chs),
                LBRACE => Token::new(TokenType::LBRACE, chs),
                RBRACE => Token::new(TokenType::RBRACE, chs),
                0 => Token::new(TokenType::EOF, chs),
                _ => Token::new(TokenType::ILLEGAL, chs),
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
    }
}
