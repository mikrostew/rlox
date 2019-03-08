use std::env;
use std::fs;
use std::io;
use std::io::Write;
use std::process::exit;

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

struct Lox {
    had_error: bool,
}

impl Lox {
    fn new() -> Self {
        Lox { had_error: false }
    }

    // Read in the given script file, and run it
    fn run_file(path: &str) {
        let script_contents =
            fs::read_to_string(path).expect(&format!("Could not read input file {}", path));
        let lox = Lox::new();
        lox.run(&script_contents);
        if lox.had_error() {
            exit(65);
        }
    }

    // Run interactively, taking in one line of input at a time and running it (like a REPL)
    fn run_prompt() {
        let mut lox = Lox::new();
        // user has to hit ^C to get out of this loop
        loop {
            let mut input = String::new();
            // print out the prompt
            print!("> ");
            match io::stdout().flush() {
                // ensures that is printed immediately (if stdout is line-buffered)
                Ok(_) => (),
                Err(e) => eprintln!("Error flushing stdout: {}", e),
            }

            // read in a line from the user
            match io::stdin().read_line(&mut input) {
                Ok(_n) => {
                    // println!("{} bytes read", n);
                    lox.run(&input);
                    lox.reset_err();
                }
                Err(error) => {
                    eprintln!("error: {}", error);
                    exit(1);
                }
            }
        }
    }

    // Run the input text
    fn run(&self, source: &str) {
        // TODO
        // Scanner scanner = new Scanner(source);
        // List<Token> tokens = scanner.scanTokens();

        // // For now, just print the tokens.
        // for (Token token : tokens) {
        //   System.out.println(token);
        // }

        // for now, just print whatever was input
        println!("{}", source);
    }

    // for error handling
    fn error(&mut self, line: u64, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: u64, err_where: &str, message: &str) {
        println!("[line {}] Error {}: {}", line, err_where, message);
        self.had_error = true;
    }

    fn had_error(&self) -> bool {
        self.had_error
    }

    fn reset_err(&mut self) {
        self.had_error = false;
    }

    // TODO: the book doesn't implement it, but I would like to be able to do something like:
    //
    // Error: Unexpected "," in argument list.
    //
    //     15 | function(first, second,);
    //                                ^-- Here.
}
