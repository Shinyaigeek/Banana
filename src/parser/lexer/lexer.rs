use std::str::CharIndices;
#[path = "../tokenizer/mod.rs"]
mod tokenizer;

// TODO is This struct field is good?
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut chars = input.char_indices();
        let (_, ch) = chars.nth(0).unwrap();
        Lexer {
            input: input,
            position: 0,
            read_position: 1,
            ch,
        }
    }

    fn read_char(&mut self) -> char {
        if self.read_position > self.input.len() {
            // TODO is this ok?
            self.ch = '0';
        } else {
            let (_, ch) = self.input.char_indices().nth(self.read_position).unwrap();
            self.ch = ch;
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
        let src = String::from("asdf");
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.ch, 'a');
        assert_eq!(lexer.read_char(), 's');
    }
}
