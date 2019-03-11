use std::fmt;
use std::iter::Peekable;
use std::str::Chars;
use std::str::FromStr;

use crate::error::Reporter;
use crate::token::{Token, TokenKind};

// keep track of current location
#[derive(Clone)]
pub struct ScanPosition {
    // TODO: will need file path?
    // current line number
    line: u64,
    // current column
    column: u64,
}

impl ScanPosition {
    pub fn new() -> Self {
        ScanPosition {
            // start at the beginning of everything
            line: 1,
            column: 0,
        }
    }

    pub fn next_char(&mut self) {
        self.column += 1;
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.column = 0;
    }

    pub fn adjust(&mut self, by_chars: u64) {
        if by_chars > 0 {
            self.column -= by_chars - 1;
        }
    }
}

impl fmt::Display for ScanPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

pub struct Scanner {
    source: String,
    // reporter that implements this trait
    err_reporter: Box<Reporter>,
    // how many errors the scanner encountered
    num_errors: u64,
}

impl Scanner {
    pub fn new(source: String, err_reporter: impl Reporter + 'static) -> Self {
        Scanner {
            source,
            err_reporter: Box::new(err_reporter),
            num_errors: 0,
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, String> {
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

        tokens.push(Token::new(TokenKind::Eof, "", &scan_position));

        if self.num_errors > 0 {
            Err(format!("Scanner encountered {} errors", self.num_errors))
        } else {
            Ok(tokens)
        }
    }

    fn advance(&self, chars: &mut Peekable<Chars>, sp: &mut ScanPosition) -> Option<char> {
        sp.next_char();
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
        let current_char = self.advance(chars, sp);
        match current_char {
            // single characters
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
                    (TokenKind::BangEqual, "!=")
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

            // this can be '/', or '// comment', or '/* ... */'
            Some('/') => {
                // '// comment' style comments
                if self.match_next('/', chars, sp) {
                    // comment goes to the end of the line (or end of string)
                    loop {
                        match chars.peek() {
                            Some(&'\n') => break,
                            Some(_) => self.advance(chars, sp),
                            None => break, // got to the end of file, OK
                        };
                    }
                    self.make_token(TokenKind::Ignore, "", sp)
                // '/* ... */' style comments
                } else if self.match_next('*', chars, sp) {
                    // comment goes to the end of the line (or end of string)
                    loop {
                        match chars.peek() {
                            Some(&'*') => {
                                // check for the end of the comment block
                                self.advance(chars, sp);
                                if chars.peek() == Some(&'/') {
                                    self.advance(chars, sp);
                                    break;
                                }
                            }
                            Some(_) => {
                                self.advance(chars, sp);
                            }
                            None => {
                                // report and return error (no token created)
                                let report_string = self.err_reporter.report(
                                    "Unterminated block comment",
                                    sp.line,
                                    sp.column,
                                    1,
                                );
                                return Err(report_string);
                            }
                        };
                    }
                    self.make_token(TokenKind::Ignore, "", sp)
                // just a slash
                } else {
                    self.make_token(TokenKind::Slash, "/", sp)
                }
            }

            // ignore whitespace
            Some(' ') | Some('\r') | Some('\t') => self.make_token(TokenKind::Ignore, "", sp),
            // handle newline
            Some('\n') => {
                sp.next_line();
                self.make_token(TokenKind::Ignore, "", sp)
            }

            // string literals
            Some('"') => self.handle_string(chars, sp),

            // number literals
            Some('0'..='9') => self.handle_number(current_char.unwrap(), chars, sp),

            // identifiers and keywords
            Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                self.handle_identifier(current_char.unwrap(), chars, sp)
            }

            Some(c) => {
                // report and return error (no token created)
                let report_string = self.err_reporter.report(
                    &format!("Unexpected character `{}`", c),
                    sp.line,
                    sp.column,
                    1, // length of single char is 1
                );
                Err(report_string)
            }
            None => Ok(None),
        }
    }

    fn handle_string(
        &self,
        chars: &mut Peekable<Chars>,
        sp: &mut ScanPosition,
    ) -> Result<Option<Token>, String> {
        let mut string_chars = String::new();
        loop {
            match chars.peek() {
                Some('"') => {
                    self.advance(chars, sp);
                    return self.make_token(
                        TokenKind::String(string_chars.clone()),
                        &format!("\"{}\"", string_chars),
                        sp,
                    );
                }
                Some(&c) => {
                    // mutli-line strings are allowed
                    if c == '\n' {
                        sp.next_line();
                    } else {
                        string_chars.push(c);
                    }
                    self.advance(chars, sp);
                }
                None => {
                    // report and return error (no token created)
                    let report_string = self.err_reporter.report(
                        "Unterminated string",
                        sp.line,
                        sp.column,
                        1, // length of single char is 1
                    );
                    return Err(report_string);
                }
            };
        }
    }

    fn handle_number(
        &self,
        digit: char,
        chars: &mut Peekable<Chars>,
        sp: &mut ScanPosition,
    ) -> Result<Option<Token>, String> {
        let mut digit_string = String::new();
        digit_string.push(digit);

        loop {
            let peeked = chars.peek();
            match peeked {
                // if if's a digit, push onto the string
                Some('0'..='9') => {
                    digit_string.push(*peeked.unwrap());
                    self.advance(chars, sp);
                }
                // if it's a decimal, only add if it's followed by another digit
                Some('.') => {
                    // TODO: to add methods to numbers, this will have to change
                    digit_string.push('.');
                    self.advance(chars, sp);
                    let peek_next = chars.peek();
                    match peek_next {
                        Some('0'..='9') => {
                            digit_string.push(*peek_next.unwrap());
                            self.advance(chars, sp);

                            loop {
                                // if there are any more digits, consume them
                                let peeked = chars.peek();
                                match peeked {
                                    Some('0'..='9') => {
                                        digit_string.push(*peeked.unwrap());
                                        self.advance(chars, sp);
                                    }
                                    // if it's anything else, just return the token, finally
                                    _ => {
                                        // parse the number
                                        let parsed_float = match f64::from_str(&digit_string) {
                                            Ok(f) => f,
                                            Err(e) => {
                                                return Err(format!(
                                                    "Could not parse number: {}",
                                                    e.to_string()
                                                ));
                                            }
                                        };
                                        return self.make_token(
                                            TokenKind::Number(parsed_float),
                                            &digit_string,
                                            sp,
                                        );
                                    }
                                }
                            }
                        }
                        // anything other than a decimal is an error
                        Some(c) => {
                            // report and return error (no token created)
                            let report_string = self.err_reporter.report(
                                &format!("Expected digit after decimal, found {}", c),
                                sp.line,
                                sp.column,
                                1,
                            );
                            return Err(report_string);
                        }
                        None => {
                            // report and return error (no token created)
                            let report_string = self.err_reporter.report(
                                "Expected digit after decimal, found EOF",
                                sp.line,
                                sp.column,
                                1,
                            );
                            return Err(report_string);
                        }
                    }
                }
                // anything else ends the number
                Some(_) => {
                    // parse the number
                    let parsed_float = match f64::from_str(&digit_string) {
                        Ok(f) => f,
                        Err(e) => {
                            return Err(format!("Could not parse number: {}", e.to_string()));
                        }
                    };
                    return self.make_token(TokenKind::Number(parsed_float), &digit_string, sp);
                }
                None => return Ok(None),
            }
        }
    }

    fn handle_identifier(
        &self,
        first_char: char,
        chars: &mut Peekable<Chars>,
        sp: &mut ScanPosition,
    ) -> Result<Option<Token>, String> {
        let mut id_string = String::new();
        id_string.push(first_char);

        loop {
            let peeked = chars.peek();
            match peeked {
                // if if's alphanumeric, push onto the string
                Some('0'..='9') | Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                    id_string.push(*peeked.unwrap());
                    self.advance(chars, sp);
                }
                // anything else means end of the identifier
                _ => {
                    // check for keywords
                    match TokenKind::get_keyword(&id_string) {
                        Some(token_kind) => return self.make_token(token_kind, &id_string, sp),
                        _ => {
                            return self.make_token(
                                TokenKind::Identifier(id_string.clone()),
                                &id_string,
                                sp,
                            );
                        }
                    }
                }
            }
        }
    }

    fn make_token(
        &self,
        kind: TokenKind,
        lexeme: &str,
        sp: &ScanPosition,
    ) -> Result<Option<Token>, String> {
        Ok(Some(Token::new(kind, lexeme, sp)))
    }
}
