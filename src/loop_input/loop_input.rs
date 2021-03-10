use std::io::{self, Read};

pub fn loop_input<CF>(mut handle_input: CF)
where
    CF: FnMut(String),
{
    let mut buffer = String::new();
    io::stdin().read_line(&mut buffer);
    handle_input(buffer);

    loop_input(handle_input);
}
