mod evaluator;
mod loop_input;
mod parser;
use crate::evaluator::evaluator::evaluate;
use crate::evaluator::variable::variable::{Environment, VariableValue};
use crate::loop_input::loop_input::loop_input;
use crate::parser::lexer::lexer::Lexer;
use crate::parser::parser::{Node, Parser};
use crate::parser::tokenizer::tokenizer::{Token, TokenType, Tokens};

fn main() {
    let mut environment = Environment::new();
    loop_input(
        |src| {
            let mut lexer = Lexer::new(&src);
            let mut tokens = Tokens::new(lexer);
            let mut parser = Parser::new(tokens);
            parser.parse();
            let node = Node::Program(parser.program);
            let mut break_flag = false;
            let result = evaluate(node, &mut environment, &mut break_flag);
            println!("{:?}", result.inspect());
        },
        "",
        0,
    );

    //     let src = String::from(
    //         "let i = 1;
    // if (1 == 1) {
    // let i = 9 * 6;
    // };
    // i + 1;",
    //     );

    //     let mut lexer = Lexer::new(&src);
    //     let mut tokens = Tokens::new(lexer);
    //     let mut parser = Parser::new(tokens);
    //     parser.parse();
    //     let node = Node::Program(parser.program);
    //     let mut environment = Environment::new();
    //     let result = evaluate(node, &mut environment);
    //     println!("{:?}", result.inspect());
}
