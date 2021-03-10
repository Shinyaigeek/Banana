pub fn loop_input<CF>(mut handle_input: CF)
where
    CF: FnMut(String),
{
    handle_input("10;".to_string())
}
