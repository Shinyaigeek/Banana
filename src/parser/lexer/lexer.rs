#[path = "../tokenizer/mod.rs"]
mod tokenizer;

// TODO is This struct field is good?
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
            read_position: 1,
            ch: bytes[0],
        }
    }

    pub fn read_char(&mut self) -> u8 {
        if self.read_position > self.input.len() {
            // TODO is this ok?
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;

        self.ch
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn read_char_works() {
        let src: String = String::from("asdf");
        let mut lexer = Lexer::new(&src);
        assert_eq!(lexer.ch, 'a' as u8);
        assert_eq!(lexer.read_char(), 's' as u8);
    }
}
