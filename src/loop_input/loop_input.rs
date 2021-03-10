pub fn loop_input<CF>(mut gets: CF)
where
    CF: FnMut(String),
{
    gets("10;".to_string())
}
