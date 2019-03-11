use std::env;
use std::fmt;
use std::fs;
use std::io;
use std::io::Write;
use std::iter::Peekable;
use std::process::exit;
use std::str::Chars;

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

struct Lox {}

impl Lox {
    fn new() -> Self {
        Lox {}
    }

    // Read in the given script file, and run it
    fn run_file(path: &str) {
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
        let err_reporter = BasicErrorReporter::new();
        let scanner = Scanner::new(source.to_string(), err_reporter);
        let tokens = scanner.scan_tokens()?;

        // TODO: For now, just print the tokens (this can be a cmd line option later)
        for token in tokens {
            println!("{:?}", token);
        }
        Ok(())
    }
}

trait ErrorReporter {
    fn report(&self, message: &str, line_num: u64, column: u64, _length: u64) -> String;
}

struct BasicErrorReporter {}

impl BasicErrorReporter {
    pub fn new() -> Self {
        BasicErrorReporter {}
    }
}

impl ErrorReporter for BasicErrorReporter {
    fn report(&self, message: &str, line_num: u64, column: u64, _length: u64) -> String {
        let error_report = format!("Error: {} [line {}:{}]", message, line_num, column);
        eprintln!("{}", error_report);
        error_report
    }
}

// TODO:
// the book doesn't implement it, but given those args it should be possible to do this:
//
// Error: Unexpected "," in argument list.
//
//     15:23 | function(first, second,);
//                                   ^-- here
//
//  OR, like cargo does:
//
// error[E0615]: attempted to take value of method `had_error` on type `&Lox`
//   --> src/main.rs:99:14
//    |
// 99 |         self.had_error
//    |              ^^^^^^^^^ help: use parentheses to call the method: `had_error()`
//
// fn error(&mut self, message: &str, line_num: u64, column: u64, _length: u64) {
//     println!("[line {}:{}] Error: {}", line_num, column, message);
// }

// the kind of token that was scanned
#[derive(PartialEq)]
enum TokenKind {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    // // Literals
    // Identifier,
    // String,
    // Number,
    // // Keywords
    // And,
    // Class,
    // Else,
    // False,
    // Fun,
    // For,
    // If,
    // Nil,
    // Or,
    // Print,
    // Return,
    // Super,
    // This,
    // True,
    // Var,
    // While,

    // Other
    Eof,

    // Tokens that will be ignored (comments, unknown chars)
    Ignore,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let to_write = match self {
            // Single-character tokens
            TokenKind::LeftParen => "LeftParen",
            TokenKind::RightParen => "RightParen",
            TokenKind::LeftBrace => "LeftBrace",
            TokenKind::RightBrace => "RightBrace",
            TokenKind::Comma => "Comma",
            TokenKind::Dot => "Dot",
            TokenKind::Minus => "Minus",
            TokenKind::Plus => "Plus",
            TokenKind::Semicolon => "Semicolon",
            TokenKind::Slash => "Slash",
            TokenKind::Star => "Star",
            // One or two character tokens
            TokenKind::Bang => "Bang",
            TokenKind::BangEqual => "BangEqual",
            TokenKind::Equal => "Equal",
            TokenKind::EqualEqual => "EqualEqual",
            TokenKind::Greater => "Greater",
            TokenKind::GreaterEqual => "GreaterEqual",
            TokenKind::Less => "Less",
            TokenKind::LessEqual => "LessEqual",
            // // Literals
            // TokenKind::Identifier => "Identifier",
            // TokenKind::String => "String",
            // TokenKind::Number => "Number",
            // // Keywords
            // TokenKind::And => "And",
            // TokenKind::Class => "Class",
            // TokenKind::Else => "Else",
            // TokenKind::False => "False",
            // TokenKind::Fun => "Fun",
            // TokenKind::For => "For",
            // TokenKind::If => "If",
            // TokenKind::Nil => "Nil",
            // TokenKind::Or => "Or",
            // TokenKind::Print => "Print",
            // TokenKind::Return => "Return",
            // TokenKind::Super => "Super",
            // TokenKind::This => "This",
            // TokenKind::True => "True",
            // TokenKind::Var => "Var",
            // TokenKind::While => "While",
            // Other
            TokenKind::Eof => "Eof",
            TokenKind::Ignore => "",
        };
        write!(f, "{}", to_write)
    }
}

struct Token {
    kind: TokenKind,
    // the raw characters from the input
    lexeme: String,
    // TODO
    // literal: how to do a generic Object? maybe optional string, int, or whatever
    // OR, some Literal<> that holds a specific one? maybe...

    // TODO: wrap this in something like TokenLocation?
    // line number where the token appears
    line: u64,
    // column where the token starts
    column: u64,
    // length of the token
    length: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, /* literal,*/ line: u64, column: u64) -> Self {
        Token {
            kind,
            lexeme: lexeme.to_string(),
            // TODO: literal,
            line,
            column,
            length: lexeme.len(),
        }
    }

    pub fn can_ignore(&self) -> bool {
        self.kind == TokenKind::Ignore
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: include the literal?
        write!(f, "{} {}", self.kind, self.lexeme /*, self.literal*/)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // TODO: include the literal?
        write!(
            f,
            "{} {} line{}:col{}:len{}",
            self.kind, self.lexeme, /*, self.literal*/ self.line, self.column, self.length
        )
    }
}

// keep track of current location
struct ScanPosition {
    // position of current character being considered
    pub current_char: usize,
    // current line number
    pub line: u64,
    // current column
    pub column: u64,
}

impl ScanPosition {
    pub fn new() -> Self {
        ScanPosition {
            // start at the beginning of everything
            current_char: 1,
            line: 1,
            column: 1,
        }
    }
}

struct Scanner {
    source: String,
    // reporter that implements this trait
    err_reporter: Box<ErrorReporter>,
    // how many errors the scanner encountered
    num_errors: u64,
}

impl Scanner {
    pub fn new(source: String, err_reporter: impl ErrorReporter + 'static) -> Self {
        Scanner {
            source,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    fn scan_tokens(mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();
        let source = self.source.clone();
        let mut chars = source.chars().peekable();
        let mut scan_position = ScanPosition::new();

        // scan tokens until EOF is reached
        loop {
            match self.scan_token(&mut chars, &mut scan_position) {
                Ok(current_token) => {
                    match current_token {
                        Some(token) => {
                            if !token.can_ignore() {
                                tokens.push(token)
                            }
                        }
                        // no more tokens, at EOF
                        None => break,
                    }
                }
                Err(_) => self.num_errors += 1,
            }
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            "",
            scan_position.line,
            scan_position.column,
        ));

        if self.num_errors > 0 {
            Err(format!("Scanner encountered {} errors", self.num_errors))
        } else {
            Ok(tokens)
        }
    }

    fn advance(&self, chars: &mut Peekable<Chars>, sp: &mut ScanPosition) -> Option<char> {
        sp.current_char += 1;
        sp.column += 1;
        chars.next()
    }

    // if the next character matches the char_to_match, consume it and return true
    fn match_next(
        &self,
        char_to_match: char,
        chars: &mut Peekable<Chars>,
        sp: &mut ScanPosition,
    ) -> bool {
        match chars.peek() {
            Some(&c) => {
                // if the next character matches, consume it and advance things accordingly
                if c == char_to_match {
                    self.advance(chars, sp);
                    true
                } else {
                    // if not, don't consume the char
                    false
                }
            }
            // if there's nothing left, it doesn't match of course
            None => false,
        }
    }

    fn scan_token(
        &self,
        chars: &mut Peekable<Chars>,
        sp: &mut ScanPosition,
    ) -> Result<Option<Token>, String> {
        match self.advance(chars, sp) {
            // just single characters
            Some('(') => self.make_token(TokenKind::LeftParen, "(", sp),
            Some(')') => self.make_token(TokenKind::RightParen, ")", sp),
            Some('{') => self.make_token(TokenKind::LeftBrace, "{", sp),
            Some('}') => self.make_token(TokenKind::RightBrace, "}", sp),
            Some(',') => self.make_token(TokenKind::Comma, ",", sp),
            Some('.') => self.make_token(TokenKind::Dot, ".", sp),
            Some('-') => self.make_token(TokenKind::Minus, "-", sp),
            Some('+') => self.make_token(TokenKind::Plus, "+", sp),
            Some(';') => self.make_token(TokenKind::Semicolon, ";", sp),
            Some('*') => self.make_token(TokenKind::Star, "*", sp),

            // these can be one or two characters
            Some('!') => {
                let (kind, lexeme) = if self.match_next('=', chars, sp) {
                    (TokenKind::BangEqual, "!-")
                } else {
                    (TokenKind::Bang, "!")
                };
                self.make_token(kind, lexeme, sp)
            }
            Some('=') => {
                let (kind, lexeme) = if self.match_next('=', chars, sp) {
                    (TokenKind::EqualEqual, "==")
                } else {
                    (TokenKind::Equal, "=")
                };
                self.make_token(kind, lexeme, sp)
            }
            Some('<') => {
                let (kind, lexeme) = if self.match_next('=', chars, sp) {
                    (TokenKind::LessEqual, "<=")
                } else {
                    (TokenKind::Less, "<")
                };
                self.make_token(kind, lexeme, sp)
            }
            Some('>') => {
                let (kind, lexeme) = if self.match_next('=', chars, sp) {
                    (TokenKind::GreaterEqual, ">=")
                } else {
                    (TokenKind::Greater, ">")
                };
                self.make_token(kind, lexeme, sp)
            }

            // this can be '/', or '// comment'
            Some('/') => {
                if self.match_next('/', chars, sp) {
                    // comment goes to the end of the line (or end of string)
                    loop {
                        match chars.peek() {
                            Some(&'\n') => break,
                            Some(_) => self.advance(chars, sp),
                            None => break,
                        };
                    }
                    self.make_token(TokenKind::Ignore, "", sp)
                } else {
                    self.make_token(TokenKind::Slash, "/", sp)
                }
            }
            // TODO: reset column and increment line number for newline
            Some(c) => {
                // report and return error (no token created)
                let report_string = self.err_reporter.report(
                    &format!("Unexpected character '{}'", c),
                    sp.line,
                    sp.column,
                    1, // length of single char is 1
                );
                Err(report_string)
            }
            None => Ok(None),
        }
    }

    fn make_token(
        &self,
        kind: TokenKind,
        lexeme: &str,
        sp: &ScanPosition,
    ) -> Result<Option<Token>, String> {
        Ok(Some(Token::new(kind, lexeme, sp.line, sp.column)))
    }
}
