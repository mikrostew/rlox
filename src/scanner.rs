use std::iter::Peekable;
use std::str::Chars;
use std::str::FromStr;

use crate::error::Reporter;
use crate::token::{Position, Token, TokenKind};

pub struct Scanner {
    source: String,
    // reporter that implements this trait
    err_reporter: Box<Reporter>,
    // how many errors the scanner encountered
    num_errors: u64,
}

impl Scanner {
    pub fn new<R: Reporter + 'static>(source: String, err_reporter: R) -> Self {
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
        let mut position = Position::new();

        // scan tokens until EOF is reached
        loop {
            match self.scan_token(&mut chars, &mut position) {
                Ok(current_token) => {
                    match current_token {
                        Some(token) => {
                            if !token.can_ignore() {
                                tokens.push(token)
                            }
                        }
                        // no more tokens, at EOF
                        None => {
                            break;
                        }
                    }
                }
                Err(_) => self.num_errors += 1,
            }
        }

        tokens.push(Token::new(TokenKind::Eof, "EOF", &position));

        if self.num_errors > 0 {
            Err(format!("scanner encountered {} error(s)", self.num_errors))
        } else {
            Ok(tokens)
        }
    }

    fn advance(&self, chars: &mut Peekable<Chars>, pos: &mut Position) -> Option<char> {
        pos.next_char();
        chars.next()
    }

    // if the next character matches the char_to_match, consume it and return true
    fn match_next(
        &self,
        char_to_match: char,
        chars: &mut Peekable<Chars>,
        pos: &mut Position,
    ) -> bool {
        match chars.peek() {
            Some(&c) => {
                // if the next character matches, consume it and advance things accordingly
                if c == char_to_match {
                    self.advance(chars, pos);
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
        pos: &mut Position,
    ) -> Result<Option<Token>, String> {
        let current_char = self.advance(chars, pos);
        match current_char {
            // single characters
            Some('(') => self.make_token(TokenKind::OpenParen, "(", pos),
            Some(')') => self.make_token(TokenKind::CloseParen, ")", pos),
            Some('{') => self.make_token(TokenKind::OpenBrace, "{", pos),
            Some('}') => self.make_token(TokenKind::CloseBrace, "}", pos),
            Some(',') => self.make_token(TokenKind::Comma, ",", pos),
            Some('.') => self.make_token(TokenKind::Dot, ".", pos),
            Some('-') => self.make_token(TokenKind::Minus, "-", pos),
            Some('+') => self.make_token(TokenKind::Plus, "+", pos),
            Some(';') => self.make_token(TokenKind::Semicolon, ";", pos),
            Some('*') => self.make_token(TokenKind::Star, "*", pos),

            // these can be one or two characters
            Some('!') => {
                let (kind, lexeme) = if self.match_next('=', chars, pos) {
                    (TokenKind::BangEqual, "!=")
                } else {
                    (TokenKind::Bang, "!")
                };
                self.make_token(kind, lexeme, pos)
            }
            Some('=') => {
                let (kind, lexeme) = if self.match_next('=', chars, pos) {
                    (TokenKind::EqualEqual, "==")
                } else {
                    (TokenKind::Equal, "=")
                };
                self.make_token(kind, lexeme, pos)
            }
            Some('<') => {
                let (kind, lexeme) = if self.match_next('=', chars, pos) {
                    (TokenKind::LessEqual, "<=")
                } else {
                    (TokenKind::Less, "<")
                };
                self.make_token(kind, lexeme, pos)
            }
            Some('>') => {
                let (kind, lexeme) = if self.match_next('=', chars, pos) {
                    (TokenKind::GreaterEqual, ">=")
                } else {
                    (TokenKind::Greater, ">")
                };
                self.make_token(kind, lexeme, pos)
            }

            // this can be '/', or '// comment', or '/* ... */'
            Some('/') => {
                // '// comment' style comments
                if self.match_next('/', chars, pos) {
                    // comment goes to the end of the line (or end of string)
                    loop {
                        match chars.peek() {
                            Some(&'\n') => break,
                            Some(_) => self.advance(chars, pos),
                            None => break, // got to the end of file, OK
                        };
                    }
                    self.make_token(TokenKind::Ignore, "", pos)
                // '/* ... */' style comments
                } else if self.match_next('*', chars, pos) {
                    // keep track of where the comment started (for error reporting)
                    let mut start_pos = pos.clone();
                    start_pos.adjust(2);
                    // comment goes to the end of the line (or end of string)
                    loop {
                        match chars.peek() {
                            Some(&'*') => {
                                // check for the end of the comment block
                                self.advance(chars, pos);
                                if chars.peek() == Some(&'/') {
                                    self.advance(chars, pos);
                                    break;
                                }
                            }
                            Some(_) => {
                                self.advance(chars, pos);
                            }
                            None => {
                                // report and return error (no token created)
                                let report_string = self.err_reporter.report(
                                    "unterminated block comment",
                                    "expected matching `*/`",
                                    &start_pos,
                                    2,
                                );
                                return Err(report_string);
                            }
                        };
                    }
                    self.make_token(TokenKind::Ignore, "", pos)
                // just a slash
                } else {
                    self.make_token(TokenKind::Slash, "/", pos)
                }
            }

            // ignore whitespace
            Some(' ') | Some('\r') | Some('\t') => self.make_token(TokenKind::Ignore, "", pos),
            // handle newline
            Some('\n') => {
                pos.next_line();
                self.make_token(TokenKind::Ignore, "", pos)
            }

            // string literals
            Some('"') => self.handle_string(chars, pos),

            // number literals
            Some('0'..='9') => self.handle_number(current_char.unwrap(), chars, pos),

            // identifiers and keywords
            Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                self.handle_identifier(current_char.unwrap(), chars, pos)
            }

            Some(c) => {
                // report and return error (no token created)
                let report_string = self.err_reporter.report(
                    &format!("unexpected character `{}`", c),
                    "unexpected char",
                    pos,
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
        pos: &mut Position,
    ) -> Result<Option<Token>, String> {
        // keep track of start posiition (for error reporting)
        let start_pos = pos.clone();

        let mut string_chars = String::new();
        loop {
            match chars.peek() {
                Some('"') => {
                    self.advance(chars, pos);
                    return self.make_token(
                        TokenKind::String(string_chars.clone()),
                        &format!("{}", string_chars),
                        pos,
                    );
                }
                Some(&c) => {
                    // mutli-line strings are allowed
                    if c == '\n' {
                        pos.next_line();
                    } else {
                        string_chars.push(c);
                    }
                    self.advance(chars, pos);
                }
                None => {
                    // report and return error (no token created)
                    let report_string = self.err_reporter.report(
                        "unterminated string",
                        "expected closing `\"`",
                        &start_pos,
                        1 + string_chars.len() as u64, // string plus the first `"`
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
        pos: &mut Position,
    ) -> Result<Option<Token>, String> {
        let mut digit_string = String::new();
        digit_string.push(digit);

        // save the start position for the token
        let start_pos = pos.clone();

        loop {
            let peeked = chars.peek();
            match peeked {
                // if if's a digit, push onto the string
                Some('0'..='9') => {
                    digit_string.push(*peeked.unwrap());
                    self.advance(chars, pos);
                }
                // if it's a decimal, only add if it's followed by another digit
                Some('.') => {
                    // TODO: to add methods to numbers, this will have to change
                    digit_string.push('.');
                    self.advance(chars, pos);
                    let peek_next = chars.peek();
                    match peek_next {
                        Some('0'..='9') => {
                            // unwrap is ok because we just checked for Some()
                            digit_string.push(*peek_next.unwrap());
                            self.advance(chars, pos);

                            loop {
                                // if there are any more digits, consume them
                                let peeked = chars.peek();
                                match peeked {
                                    Some('0'..='9') => {
                                        digit_string.push(*peeked.unwrap());
                                        self.advance(chars, pos);
                                    }
                                    // if it's anything else, just return the token, finally
                                    _ => {
                                        let parsed_float =
                                            self.parse_number(&digit_string, &start_pos)?;
                                        return self.make_token(
                                            TokenKind::Number(parsed_float),
                                            &digit_string,
                                            pos,
                                        );
                                    }
                                }
                            }
                        }
                        // anything other than a decimal is an error
                        Some(c) => {
                            // report the right location
                            pos.next_char();
                            // report and return error (no token created)
                            let report_string = self.err_reporter.report(
                                &format!("expected digit after decimal, found `{}`", c),
                                "expected digit",
                                pos,
                                1,
                            );
                            return Err(report_string);
                        }
                        None => {
                            // report the right location
                            pos.next_char();
                            // report and return error (no token created)
                            let report_string = self.err_reporter.report(
                                "expected digit after decimal, found EOF",
                                "expected digit",
                                pos,
                                1,
                            );
                            return Err(report_string);
                        }
                    }
                }
                // anything else or EOF ends the number
                Some(_) | None => {
                    let parsed_float = self.parse_number(&digit_string, &start_pos)?;
                    return self.make_token(TokenKind::Number(parsed_float), &digit_string, pos);
                }
            }
        }
    }

    fn parse_number(&self, digit_string: &String, pos: &Position) -> Result<f64, String> {
        f64::from_str(&digit_string).map_err(|e| {
            // figure out the right position and length
            let length = digit_string.len() as u64;

            let report_string = self.err_reporter.report(
                &format!("Could not parse number: {}", e.to_string()),
                "could not parse",
                pos,
                length,
            );
            report_string
        })
    }

    fn handle_identifier(
        &self,
        first_char: char,
        chars: &mut Peekable<Chars>,
        pos: &mut Position,
    ) -> Result<Option<Token>, String> {
        let mut id_string = String::new();
        id_string.push(first_char);

        loop {
            let peeked = chars.peek();
            match peeked {
                // if if's alphanumeric, push onto the string
                Some('0'..='9') | Some('a'..='z') | Some('A'..='Z') | Some('_') => {
                    id_string.push(*peeked.unwrap());
                    self.advance(chars, pos);
                }
                // anything else means end of the identifier
                _ => {
                    // check for keywords
                    match TokenKind::get_keyword(&id_string) {
                        Some(token_kind) => return self.make_token(token_kind, &id_string, pos),
                        _ => {
                            return self.make_token(
                                TokenKind::Identifier(id_string.clone()),
                                &id_string,
                                pos,
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
        pos: &Position,
    ) -> Result<Option<Token>, String> {
        Ok(Some(Token::new(kind, lexeme, pos)))
    }
}
