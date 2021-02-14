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

pub enum Token {
    EOF,
    ASSIGN,
    PLUS,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE
}

pub fn tokenize(char: u8) -> u8 {
    let token = match char {
        ASSIGN => ASSIGN,
        PLUS => PLUS,
        COMMA => COMMA,
        SEMICOLON => SEMICOLON,
        LPAREN => LPAREN,
        RPAREN => RPAREN,
        LBRACE => LBRACE,
        RBRACE => RBRACE,
        _ => {
            EOF
        }
    };

    token
}

pub struct Tokens {
    
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn tokenize_works() {

        let mut lexer = Lexer::new(&String::from("(){}+"));

        assert_eq!(tokenize(lexer.read_char()), LPAREN);
    }
}

