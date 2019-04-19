use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

use crate::ast::AstPrinter;
use crate::error;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::resolver::Resolver;
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
        // TODO: error reporter for this
        let mut interpreter = Interpreter::new();
        match lox.run(Some(path.to_string()), &script_contents, &mut interpreter) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("Error: {}", e);
                // for runtime error, exit with code 70
                exit(65);
            }
        }
    }

    // Run interactively, taking in one line of input at a time and running it (like a REPL)
    pub fn run_prompt() {
        let mut lox = Lox::new();
        // TODO: error reporter for this
        let mut interpreter = Interpreter::new();
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
                    let trimmed = input.trim();
                    // TODO: it would be nice to support entering an expression and printing the resulting value, instead of requiring a statement
                    // (see http://www.craftinginterpreters.com/statements-and-state.html#challenges)
                    match lox.run(None, trimmed, &mut interpreter) {
                        Ok(_) => (),
                        Err(e) => {
                            // print the error, but don't exit the interpreter
                            eprintln!("error: {}", e);
                            eprintln!("");
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
    fn run(
        &mut self,
        file: Option<String>,
        source: &str,
        interpreter: &mut Interpreter,
    ) -> Result<(), String> {
        // TODO: add debug command line option --debug-source
        println!("source:");
        println!("```");
        println!("{}", source);
        println!("```");
        println!("");

        // TODO: creating 3 reporters here, which I shouldn't need to do
        // (could probably use Rc to fix this...)
        let err_reporter1 = error::BetterReporter::new(source.to_string());
        let scanner = Scanner::new(file.clone(), source.to_string(), err_reporter1);
        let tokens = scanner.scan_tokens()?;

        // TODO: add this as a command line option (--show-tokens or --debug-tokens)
        // println!("tokens:");
        // for token in &tokens {
        //     println!("{:?}", token);
        // }
        // println!("");

        let err_reporter2 = error::BetterReporter::new(source.to_string());
        let parser = Parser::new(file.clone(), tokens, err_reporter2);
        let mut statements = parser.parse()?;

        let err_reporter3 = error::BetterReporter::new(source.to_string());
        let mut resolver = Resolver::new(err_reporter3);
        resolver.resolve(&mut statements)?;

        // TODO: add this as a command line option (--show-ast or --debug-ast)
        let mut ast_printer = AstPrinter::new();
        println!("ast:");
        println!("{}", ast_printer.print(&statements)?);
        println!("");

        println!("result:");
        interpreter.interpret(statements)?;
        println!("");

        Ok(())
    }
}
