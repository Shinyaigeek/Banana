// TODO is This struct field is good?
#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: &String) -> Self {
        let bytes = &input.as_bytes();
        Lexer {
            input: input.to_string(),
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }

    pub fn read_char(&mut self) -> u8 {
        if self.read_position >= self.input.len() {
            // TODO is this ok?
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.ch
    }

    pub fn peek(&self) -> u8 {
        self.input.as_bytes()[self.read_position]
    }

    pub fn read_identifier(&mut self) -> Vec<u8> {
        vec![1]
    }

    pub fn is_letter(ch: u8) -> bool {
        (b'a' <= ch && ch <= b'z') || (b'A' <= ch && ch <= b'Z') || ch == b'_'
    }

    pub fn is_identifier_end(ch: u8) -> bool {
        ch == b' ' || ch == b'\t' || ch == b'\n' || ch == b'\r' || ch == b';'
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn read_char_works() {
        let src: String = String::from("asdf");
        let mut lexer = Lexer::new(&src);
        assert_eq!(lexer.read_char(), 'a' as u8);
        assert_eq!(lexer.read_char(), 's' as u8);
    }

    #[test]
    fn peek_works() {
        let src: String = String::from("asdf");
        let mut lexer = Lexer::new(&src);
        assert_eq!(lexer.peek(), 'a' as u8);
        assert_eq!(lexer.peek(), 'a' as u8);
    }

    #[test]
    fn is_letter_works() {
        assert!(Lexer::is_letter(b'c'));
        assert!(!Lexer::is_letter(b'5'));
        assert!(!Lexer::is_letter(b' '));
        assert!(!Lexer::is_letter(b'['));
    }

    #[test]
    fn is_identifier_end_works() {
        assert!(!Lexer::is_identifier_end(b'c'));
        assert!(!Lexer::is_identifier_end(b'5'));
        assert!(Lexer::is_identifier_end(b' '));
        assert!(!Lexer::is_identifier_end(b'['));
    }
}
