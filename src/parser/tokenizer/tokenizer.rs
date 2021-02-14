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

#[derive(Debug, PartialEq)]
pub enum Token {
    EOF,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
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
            let token = Tokens::tokenize_char(ch);

            if token == Token::EOF {
                self.tokens.push(Token::EOF);
                break;
            } else {
                self.tokens.push(token);
            }
        }
    }

    fn tokenize_char(char: u8) -> Token {
        let token = match char {
            ASSIGN => Token::ASSIGN,
            PLUS => Token::PLUS,
            COMMA => Token::COMMA,
            SEMICOLON => Token::SEMICOLON,
            LPAREN => Token::LPAREN,
            RPAREN => Token::RPAREN,
            LBRACE => Token::LBRACE,
            RBRACE => Token::RBRACE,
            _ => Token::EOF,
        };

        token
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
                Token::LPAREN,
                Token::RPAREN,
                Token::LBRACE,
                Token::RBRACE,
                Token::PLUS,
                Token::EOF
            ]
        );
    }
}
