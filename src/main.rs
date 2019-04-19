use std::env;
use std::process::exit;

mod ast;
mod environment;
mod error;
mod interpreter;
mod lox;
mod parser;
mod resolver;
mod scanner;
mod token;

use lox::Lox;

fn main() {
    let args: Vec<String> = env::args().collect();

    // This only takes one optional argument - the name of the script to run
    // (first arg is the executable, second will be the script)
    if args.len() > 2 {
        eprintln!("Usage: rlox [script]");
        exit(64);
    } else if args.len() == 2 {
        // if the script is given, use that
        Lox::run_file(&args[1]);
    } else {
        // if the script is not given, enter the interactive prompt
        Lox::run_prompt();
    }
}
