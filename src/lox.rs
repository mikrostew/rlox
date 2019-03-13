use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

use crate::ast::AstPrinter;
use crate::error;
use crate::parser::Parser;
use crate::scanner::Scanner;

pub struct Lox {}

impl Lox {
    fn new() -> Self {
        Lox {}
    }

    // Read in the given script file, and run it
    pub fn run_file(path: &str) {
        let script_contents =
            fs::read_to_string(path).expect(&format!("Could not read input file {}", path));
        let mut lox = Lox::new();
        match lox.run(&script_contents) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("Error: {}", e);
                exit(65);
            }
        }
    }

    // Run interactively, taking in one line of input at a time and running it (like a REPL)
    pub fn run_prompt() {
        let mut lox = Lox::new();
        // user has to hit ^C to get out of this loop
        loop {
            let mut input = String::new();
            // print out the prompt
            print!("> ");
            match io::stdout().flush() {
                // ensures that is printed immediately (if stdout is line-buffered)
                Ok(_) => (),
                Err(e) => eprintln!("Error: could not flush stdout: {}", e),
            }

            // read in a line from the user
            match io::stdin().read_line(&mut input) {
                Ok(_n) => {
                    // println!("{} bytes read", _n);
                    match lox.run(&input) {
                        Ok(_) => (),
                        Err(e) => {
                            // print the error, but don't exit the interpreter
                            eprintln!("Error: {}", e);
                        }
                    }
                }
                Err(e) => {
                    eprintln!("Error: could not read input line: {}", e);
                    exit(1);
                }
            }
        }
    }

    // Run the input text
    fn run(&mut self, source: &str) -> Result<(), String> {
        let err_reporter = error::BasicReporter::new();

        let scanner = Scanner::new(source.to_string(), err_reporter);
        let tokens = scanner.scan_tokens()?;
        // TODO: add this as a command line option (--show-tokens or --debug-tokens)
        // for token in tokens {
        //     println!("{:?}", token);
        // }

        let parser = Parser::new(tokens, err_reporter);
        let expression = parser.parse()?;

        // TODO: add this as a command line option (--show-ast or --debug-ast)
        let ast_printer = AstPrinter::new();
        println!("ast: {}", ast_printer.print(expression));

        Ok(())
    }
}
