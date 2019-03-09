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
        let mut lox = Lox::new();
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
    fn run(&mut self, source: &str) {
        let mut scanner = Scanner::new(source.to_string(), self);
        let tokens = scanner.scan_tokens();

        // For now, just print the tokens.
        for token in tokens {
            println!("{:?}", token);
        }
    }

    // for error handling
    fn error(&mut self, message: &str, line_num: u64, column: u64, _length: u64) {
        // TODO:
        // the book doesn't implement it, but given those args it should be possible to do this:
        //
        // Error: Unexpected "," in argument list.
        //
        //     15:23 | function(first, second,);
        //                                   ^-- here
        println!("[line {}:{}] Error: {}", line_num, column, message);
        self.had_error = true;
    }

    fn had_error(&self) -> bool {
        self.had_error
    }

    fn reset_err(&mut self) {
        self.had_error = false;
    }
}

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
    length: u64,
}

impl Token {
    pub fn new(
        kind: TokenKind,
        lexeme: &str,
        /* literal,*/ line: u64,
        column: u64,
        length: u64,
    ) -> Self {
        Token {
            kind,
            lexeme: lexeme.to_string(),
            // TODO: literal,
            line,
            column,
            length,
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

struct Scanner<'a> {
    source: String,
    // source_chars: Option<Chars<'a>>,
    // tokens: Vec<Token>,

    // keep track of current location and lexeme we're scanning
    lexeme: String,
    // position of first character in the current lexeme
    lexeme_start: usize,
    // length of current lexeme
    lexeme_length: u64,
    // position of current character being considered
    current_char: usize,
    // current line number
    line: u64,
    // current column
    column: u64,

    // TODO: could I pass in something else, that only does error reporting, instead of a ref to
    // everything?
    lox: &'a mut Lox, // for error reporting
}

impl<'a> Scanner<'a> {
    pub fn new(source: String, lox: &'a mut Lox) -> Self {
        Scanner {
            source,
            // source_chars: None,
            // tokens: Vec::new(),
            lexeme: String::new(),
            lexeme_start: 0,
            lexeme_length: 0,
            current_char: 0,
            line: 1,
            column: 0,
            lox,
        }
    }

    fn scan_tokens(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let source = self.source.clone();
        let mut chars = source.chars().peekable();

        // scan tokens until EOF is reached
        loop {
            // We are at the beginning of the next lexeme.
            self.lexeme = String::new();
            self.lexeme_start = self.current_char;
            self.lexeme_length = 0;

            let current_token = self.scan_token(&mut chars);

            match current_token {
                // TODO: ignore some tokens - comment
                Some(token) => {
                    if !token.can_ignore() {
                        tokens.push(token)
                    }
                }
                // no more tokens, at EOF
                None => break,
            }
        }

        tokens.push(Token::new(
            TokenKind::Eof,
            "",
            self.line,
            self.column,
            0, // length 0
        ));

        tokens
    }

    fn error(&mut self, message: &str) {
        self.lox
            .error(message, self.line, self.column, self.lexeme_length);
    }

    fn advance(&mut self, chars: &mut Peekable<Chars>) -> Option<char> {
        self.current_char += 1;
        self.lexeme_length += 1;
        self.column += 1;
        chars.next()
    }

    // if the next character matches the char_to_match, consume it and return true
    fn match_next(&mut self, char_to_match: char, chars: &mut Peekable<Chars>) -> bool {
        match chars.peek() {
            Some(&c) => {
                // if the next character matches, consume it and advance things accordingly
                if c == char_to_match {
                    self.advance(chars);
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

    fn scan_token(&mut self, chars: &mut Peekable<Chars>) -> Option<Token> {
        match self.advance(chars) {
            // just single characters
            Some('(') => self.make_token(TokenKind::LeftParen),
            Some(')') => self.make_token(TokenKind::RightParen),
            Some('{') => self.make_token(TokenKind::LeftBrace),
            Some('}') => self.make_token(TokenKind::RightBrace),
            Some(',') => self.make_token(TokenKind::Comma),
            Some('.') => self.make_token(TokenKind::Dot),
            Some('-') => self.make_token(TokenKind::Minus),
            Some('+') => self.make_token(TokenKind::Plus),
            Some(';') => self.make_token(TokenKind::Semicolon),
            Some('*') => self.make_token(TokenKind::Star),

            // these can be one or two characters
            Some('!') => {
                let kind = if self.match_next('=', chars) {
                    TokenKind::BangEqual
                } else {
                    TokenKind::Bang
                };
                self.make_token(kind)
            }
            Some('=') => {
                let kind = if self.match_next('=', chars) {
                    TokenKind::EqualEqual
                } else {
                    TokenKind::Equal
                };
                self.make_token(kind)
            }
            Some('<') => {
                let kind = if self.match_next('=', chars) {
                    TokenKind::LessEqual
                } else {
                    TokenKind::Less
                };
                self.make_token(kind)
            }
            Some('>') => {
                let kind = if self.match_next('=', chars) {
                    TokenKind::GreaterEqual
                } else {
                    TokenKind::Greater
                };
                self.make_token(kind)
            }

            // TODO: this can be '/', or '// comment'
            Some('/') => {
                if self.match_next('/', chars) {
                    // comment goes to the end of the line (or end of string)
                    loop {
                        println!("current char: {:?}", chars.peek());
                        match chars.peek() {
                            Some(&'\n') => break,
                            Some(_) => self.advance(chars),
                            None => break,
                        };
                    }
                    self.make_token(TokenKind::Ignore)
                } else {
                    self.make_token(TokenKind::Slash)
                }
            }
            // TODO: reset column and increment line number for newline
            // TODO
            Some(c) => {
                self.error(&format!("Unexpected character '{}'", c));
                self.make_token(TokenKind::Ignore)
            }
            None => None,
        }
    }

    fn make_token(&self, kind: TokenKind) -> Option<Token> {
        Some(Token::new(
            kind,
            &self.lexeme,
            self.line,
            self.column,
            self.lexeme_length,
        ))
    }
}
