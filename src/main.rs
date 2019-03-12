use std::env;
use std::process::exit;

mod ast;
mod error;
mod lox;
mod scanner;
mod token;

// use ast::{Expr, Literal};
// use token::{Token, TokenKind};

fn main() {
    let args: Vec<String> = env::args().collect();

    // TODO: remove this section, but first do this for testing, like in the book
    // let sp = scanner::ScanPosition::new();
    // let expression = Expr::Binary(
    //     Box::new(Expr::Unary(
    //         Token::new(TokenKind::Minus, "-", &sp),
    //         Box::new(Expr::Literal(Literal::Number(123.0))),
    //     )),
    //     Token::new(TokenKind::Star, "*", &sp),
    //     Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
    //         45.67,
    //     ))))),
    // );
    // let printer = ast::AstPrinter::new();
    // print!("ast: {}", printer.print(expression));
    // exit(0);

    // This only takes one optional argument - the name of the script to run
    // (first arg is the executable, second will be the script)
    if args.len() > 2 {
        eprintln!("Usage: rlox [script]");
        exit(64);
    } else if args.len() == 2 {
        // if the script is given, use that
        lox::Lox::run_file(&args[1]);
    } else {
        // if the script is not given, enter the interactive prompt
        lox::Lox::run_prompt();
    }
}
