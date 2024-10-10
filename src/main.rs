use crate::inpret::setup;

use crate::parser::{create_ast, tokenize};

mod inpret;
mod parser;

fn main() {
    let arg = std::env::args().nth(1).unwrap();
    let program = std::fs::read_to_string(format!("programs/{}.csp", arg)).unwrap();

    let tokens = tokenize(&program);
    let ast = match create_ast(&tokens) {
        Ok(v) => v,
        Err(e) => panic!("{}", e.to_string()),
    };

    match setup(ast) {
        Err(e) => panic!("{}", e.to_string()),
        _ => {}
    };
}
