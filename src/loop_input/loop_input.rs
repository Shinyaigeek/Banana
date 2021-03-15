use std::io::{self, Read};

pub fn loop_input<CF>(mut handle_input: CF, src: &str, depth: i32)
where
    CF: FnMut(String),
{
    let mut buffer = String::new();
    let mut src = String::from(src);
    io::stdin().read_line(&mut buffer);

    src.push_str(&buffer);
    let depth_diff = count_char_in_string('{', &buffer) - count_char_in_string('}', &buffer);

    if depth + depth_diff == 0 {
        handle_input(src);

        loop_input(handle_input, "", 0);
    } else {
        loop_input(handle_input, &src, depth + depth_diff);
    }
}

fn count_char_in_string(c: char, s: &str) -> i32 {
    let mut res = 0;
    for ch in s.chars() {
        if c == ch {
            res += 1;
        }
    }

    res
}
