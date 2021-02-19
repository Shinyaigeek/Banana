use crate::parser::lexer::lexer::Lexer;

pub const EOF: u8 = 0;
pub const ASSIGN: u8 = b'=';
pub const PLUS: u8 = b'+';
pub const MINUS: u8 = b'-';
pub const ASTERISK: u8 = b'*';
pub const SLASH: u8 = b'/';
pub const PERIOD: u8 = b'.';
pub const COMMA: u8 = b',';
pub const COLON: u8 = b':';
pub const SEMICOLON: u8 = b';';
pub const LPAREN: u8 = b'(';
pub const RPAREN: u8 = b')';
pub const LBRACE: u8 = b'{';
pub const RBRACE: u8 = b'}';
pub const LBRACKET: u8 = b'[';
pub const RBRACKET: u8 = b']';
pub const GRATER: u8 = b'>';
pub const LESS: u8 = b'<';
pub const QUOTE: u8 = b'"';
pub const SQUOTE: u8 = b'\'';
pub const TQUOTE: u8 = b'`';
pub const EXCLAMATION: u8 = b'!';

pub const LET: &str = "let";
pub const FUNCTION: &str = "fn";
pub const IDENTIFIER: &str = "IDENTIFIER";
pub const INT: &str = "INT";
pub const RETURN: &str = "return";
pub const TRUE: &str = "true";
pub const FALSE: &str = "false";
pub const IF: &str = "if";
pub const ELSE: &str = "else";
pub const EQUAL: &str = "==";
pub const NOT_EQUAL: &str = "!=";
pub const MUTATE: &str = "mut";
pub const GRATER_EQUAL: &str = ">=";
pub const LESS_EQUAL: &str = "<=";
pub const ARROW: &str = "=>";

pub const ILLEGAL: &str = "ILLEGAL";

#[derive(Debug, PartialEq)]
pub enum TokenType {
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    ASTERISK,
    SLASH,
    PERIOD,
    COMMA,
    COLON,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    GRATER,
    LESS,
    QUOTE,
    SQUOTE,
    TQUOTE,
    EXCLAMATION,
    LET,
    FUNCTION,
    IDENTIFIER,
    INT,
    RETURN,
    TRUE,
    FALSE,
    IF,
    ELSE,
    MUTATE,
    EQUAL,
    NOT_EQUAL,
    GRATER_EQUAL,
    LESS_EQUAL,
    ARROW,
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
    read_position: usize,
    position: usize,
}

impl Tokens {
    pub fn new(lexer: Lexer) -> Self {
        let mut token = Tokens {
            tokens: Vec::new(),
            lexer,
            read_position: 0,
            position: 0,
        };

        token.tokenize();

        token
    }

    pub fn read_token(&mut self) -> &Token {
        if self.read_position >= self.tokens.len() {
            // TODO is this ok?
            panic!("there is no more token!!");
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.tokens.get(self.position).unwrap()
    }

    pub fn peek_token(&self) -> &Token {
        if self.read_position >= self.tokens.len() {
            // TODO is this ok?
            panic!("there is no more token!!");
        }

        self.tokens.get(self.read_position).unwrap()
    }

    fn tokenize(&mut self) {
        loop {
            let ch = self.lexer.peek();

            let token = match ch {
                ASSIGN => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* ==
                    if next_ch == ASSIGN {
                        Token::new(TokenType::EQUAL, vec![ch, self.lexer.read_char()])
                        //* =>
                    } else if next_ch == GRATER {
                        Token::new(TokenType::ARROW, vec![ch, self.lexer.read_char()])
                    } else {
                        Token::new(TokenType::ASSIGN, vec![ch])
                    }
                }
                EXCLAMATION => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* !=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::NOT_EQUAL, vec![ch, self.lexer.read_char()])
                        //* !
                    } else {
                        Token::new(TokenType::EXCLAMATION, vec![ch])
                    }
                }
                PLUS => Token::new(TokenType::PLUS, vec![self.lexer.read_char()]),
                MINUS => Token::new(TokenType::MINUS, vec![self.lexer.read_char()]),
                ASTERISK => Token::new(TokenType::ASTERISK, vec![self.lexer.read_char()]),
                SLASH => Token::new(TokenType::SLASH, vec![self.lexer.read_char()]),
                PERIOD => Token::new(TokenType::PERIOD, vec![self.lexer.read_char()]),
                COMMA => Token::new(TokenType::COMMA, vec![self.lexer.read_char()]),
                COLON => Token::new(TokenType::COLON, vec![self.lexer.read_char()]),
                SEMICOLON => Token::new(TokenType::SEMICOLON, vec![self.lexer.read_char()]),
                LPAREN => Token::new(TokenType::LPAREN, vec![self.lexer.read_char()]),
                RPAREN => Token::new(TokenType::RPAREN, vec![self.lexer.read_char()]),
                LBRACE => Token::new(TokenType::LBRACE, vec![self.lexer.read_char()]),
                RBRACE => Token::new(TokenType::RBRACE, vec![self.lexer.read_char()]),
                LBRACKET => Token::new(TokenType::LBRACKET, vec![self.lexer.read_char()]),
                RBRACKET => Token::new(TokenType::RBRACKET, vec![self.lexer.read_char()]),
                LESS => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* <=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::LESS_EQUAL, vec![ch, self.lexer.read_char()])
                    } else {
                        //* <
                        Token::new(TokenType::LESS, vec![ch])
                    }
                }
                GRATER => {
                    let ch = self.lexer.read_char();
                    let next_ch = self.lexer.peek();
                    //* >=
                    if next_ch == ASSIGN {
                        Token::new(TokenType::GRATER_EQUAL, vec![ch, self.lexer.read_char()])
                    } else {
                        //* >
                        Token::new(TokenType::GRATER, vec![ch])
                    }
                }
                QUOTE => Token::new(TokenType::QUOTE, vec![self.lexer.read_char()]),
                SQUOTE => Token::new(TokenType::SQUOTE, vec![self.lexer.read_char()]),
                TQUOTE => Token::new(TokenType::TQUOTE, vec![self.lexer.read_char()]),
                0 => Token::new(TokenType::EOF, vec![self.lexer.read_char()]),
                _ => {
                    if Lexer::is_letter(ch) {
                        // TODO :thinking_face:
                        let identifier = self.lexer.read_identifier();
                        let identifier = String::from_utf8(identifier).unwrap();
                        let identifier: &str = &identifier;
                        let token = match identifier {
                            LET => Token::new(TokenType::LET, identifier.as_bytes().to_vec()),
                            FUNCTION => {
                                Token::new(TokenType::FUNCTION, identifier.as_bytes().to_vec())
                            }
                            IF => Token::new(TokenType::IF, identifier.as_bytes().to_vec()),
                            ELSE => Token::new(TokenType::ELSE, identifier.as_bytes().to_vec()),
                            TRUE => Token::new(TokenType::TRUE, identifier.as_bytes().to_vec()),
                            FALSE => Token::new(TokenType::FALSE, identifier.as_bytes().to_vec()),
                            MUTATE => Token::new(TokenType::MUTATE, identifier.as_bytes().to_vec()),
                            RETURN => Token::new(TokenType::RETURN, identifier.as_bytes().to_vec()),
                            _ => Token::new(TokenType::IDENTIFIER, identifier.as_bytes().to_vec()),
                        };

                        token
                    } else if Lexer::is_digit(ch) {
                        let token = Token::new(TokenType::INT, self.lexer.read_number());

                        token
                    } else {
                        Token::new(TokenType::ILLEGAL, vec![self.lexer.read_char()])
                    }
                }
            };

            self.lexer.eat_white_space();

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
    fn read_token_works() {
        let src = String::from("let five = 5;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);

        assert_eq!(
            *tokens.read_token(),
            Token::__raw_new_(TokenType::LET, String::from("let"))
        );

        assert_eq!(
            *tokens.read_token(),
            Token::__raw_new_(TokenType::IDENTIFIER, String::from("five"))
        );
    }

    #[test]
    fn peek_token_works() {
        let src = String::from("let five = 5;");
        let mut lexer = Lexer::new(&src);
        let mut tokens = Tokens::new(lexer);

        assert_eq!(
            *tokens.read_token(),
            Token::__raw_new_(TokenType::LET, String::from("let"))
        );

        assert_eq!(
            *tokens.read_token(),
            Token::__raw_new_(TokenType::IDENTIFIER, String::from("five"))
        );
    }
    #[test]
    fn tokenize_works() {
        let mut lexer = Lexer::new(&String::from("(){}[]+-*/\"'`:;.,"));

        let tokens = Tokens::new(lexer);

        assert_eq!(
            tokens.tokens,
            vec![
                Token::new(TokenType::LPAREN, vec![b'(']),
                Token::new(TokenType::RPAREN, vec![b')']),
                Token::new(TokenType::LBRACE, vec![b'{']),
                Token::new(TokenType::RBRACE, vec![b'}']),
                Token::new(TokenType::LBRACKET, vec![b'[']),
                Token::new(TokenType::RBRACKET, vec![b']']),
                Token::new(TokenType::PLUS, vec![b'+']),
                Token::new(TokenType::MINUS, vec![b'-']),
                Token::new(TokenType::ASTERISK, vec![b'*']),
                Token::new(TokenType::SLASH, vec![b'/']),
                Token::new(TokenType::QUOTE, vec![b'"']),
                Token::new(TokenType::SQUOTE, vec![b'\'']),
                Token::new(TokenType::TQUOTE, vec![b'`']),
                Token::new(TokenType::COLON, vec![b':']),
                Token::new(TokenType::SEMICOLON, vec![b';']),
                Token::new(TokenType::PERIOD, vec![b'.']),
                Token::new(TokenType::COMMA, vec![b',']),
                Token::new(TokenType::EOF, vec![0]),
            ]
        );

        let mut lexer = Lexer::new(&String::from(
            "let five = 5;
        let ten = 10;
        
        fn add(left, right) {
            return five + ten;
        }
        
        add(five, ten)",
        ));
        let tokens = Tokens::new(lexer);
        assert_eq!(
            tokens.tokens,
            vec![
                Token::__raw_new_(TokenType::LET, String::from("let")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
                Token::__raw_new_(TokenType::ASSIGN, String::from("=")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::LET, String::from("let")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
                Token::__raw_new_(TokenType::ASSIGN, String::from("=")),
                Token::__raw_new_(TokenType::INT, String::from("10")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::FUNCTION, String::from("fn")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("add")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("left")),
                Token::__raw_new_(TokenType::COMMA, String::from(",")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("right")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
                Token::__raw_new_(TokenType::PLUS, String::from("+")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("add")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("five")),
                Token::__raw_new_(TokenType::COMMA, String::from(",")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("ten")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::new(TokenType::EOF, vec![0]),
            ]
        );

        let mut lexer = Lexer::new(&String::from(
            "let mut three = 3;
            if(three == 3) {
                return 3;
            }else if(three > 5){
                return 5;
            }else if(three >= 5){
                return 7;
            }else if(three < 9){
                return 9;
            }else if(three <= 9){
                return 9;
            }else{
                return 5;
            }",
        ));

        let tokens = Tokens::new(lexer);

        assert_eq!(
            tokens.tokens,
            vec![
                Token::__raw_new_(TokenType::LET, String::from("let")),
                Token::__raw_new_(TokenType::MUTATE, String::from("mut")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::ASSIGN, String::from("=")),
                Token::__raw_new_(TokenType::INT, String::from("3")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::EQUAL, String::from("==")),
                Token::__raw_new_(TokenType::INT, String::from("3")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("3")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::ELSE, String::from("else")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::GRATER, String::from(">")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::ELSE, String::from("else")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::GRATER_EQUAL, String::from(">=")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("7")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::ELSE, String::from("else")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::LESS, String::from("<")),
                Token::__raw_new_(TokenType::INT, String::from("9")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("9")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::ELSE, String::from("else")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::LESS_EQUAL, String::from("<=")),
                Token::__raw_new_(TokenType::INT, String::from("9")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("9")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::ELSE, String::from("else")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::new(TokenType::EOF, vec![0]),
            ]
        );

        let mut lexer = Lexer::new(&String::from("let mut three = [1, 3, 5]"));

        let tokens = Tokens::new(lexer);

        assert_eq!(
            tokens.tokens,
            vec![
                Token::__raw_new_(TokenType::LET, String::from("let")),
                Token::__raw_new_(TokenType::MUTATE, String::from("mut")),
                Token::__raw_new_(TokenType::IDENTIFIER, String::from("three")),
                Token::__raw_new_(TokenType::ASSIGN, String::from("=")),
                Token::__raw_new_(TokenType::LBRACKET, String::from("[")),
                Token::__raw_new_(TokenType::INT, String::from("1")),
                Token::__raw_new_(TokenType::COMMA, String::from(",")),
                Token::__raw_new_(TokenType::INT, String::from("3")),
                Token::__raw_new_(TokenType::COMMA, String::from(",")),
                Token::__raw_new_(TokenType::INT, String::from("5")),
                Token::__raw_new_(TokenType::RBRACKET, String::from("]")),
                Token::new(TokenType::EOF, vec![0]),
            ]
        );

        let mut lexer = Lexer::new(&String::from(
            "if (true) {
                return false;
            }
            
            if !(true) {
                return false;
            }
            
            if(3 != 9){
                return false
            }",
        ));

        let tokens = Tokens::new(lexer);

        assert_eq!(
            tokens.tokens,
            vec![
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::TRUE, String::from("true")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::FALSE, String::from("false")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::EXCLAMATION, String::from("!")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::TRUE, String::from("true")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::FALSE, String::from("false")),
                Token::__raw_new_(TokenType::SEMICOLON, String::from(";")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::__raw_new_(TokenType::IF, String::from("if")),
                Token::__raw_new_(TokenType::LPAREN, String::from("(")),
                Token::__raw_new_(TokenType::INT, String::from("3")),
                Token::__raw_new_(TokenType::NOT_EQUAL, String::from("!=")),
                Token::__raw_new_(TokenType::INT, String::from("9")),
                Token::__raw_new_(TokenType::RPAREN, String::from(")")),
                Token::__raw_new_(TokenType::LBRACE, String::from("{")),
                Token::__raw_new_(TokenType::RETURN, String::from("return")),
                Token::__raw_new_(TokenType::FALSE, String::from("false")),
                Token::__raw_new_(TokenType::RBRACE, String::from("}")),
                Token::new(TokenType::EOF, vec![0]),
            ]
        )
    }
}
