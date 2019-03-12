use std::fmt;

use crate::scanner::ScanPosition;

// the kind of token that was scanned
#[derive(PartialEq)]
pub enum TokenKind {
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
    // Literals
    Identifier(String),
    String(String),
    Number(f64),
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,
    // Other
    Eof,
    // Tokens that will be ignored (comments, unknown chars)
    Ignore,
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // for things that need more formatting
        let format;

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
            // Literals
            TokenKind::Identifier(s) => {
                format = format!("Identifier({})", s);
                &format
            }
            TokenKind::String(s) => {
                format = format!("String({})", s);
                &format
            }
            TokenKind::Number(n) => {
                format = format!("Number({})", n);
                &format
            }
            // Keywords
            TokenKind::And => "And",
            TokenKind::Class => "Class",
            TokenKind::Else => "Else",
            TokenKind::False => "False",
            TokenKind::Fun => "Fun",
            TokenKind::For => "For",
            TokenKind::If => "If",
            TokenKind::Nil => "Nil",
            TokenKind::Or => "Or",
            TokenKind::Print => "Print",
            TokenKind::Return => "Return",
            TokenKind::Super => "Super",
            TokenKind::This => "This",
            TokenKind::True => "True",
            TokenKind::Var => "Var",
            TokenKind::While => "While",
            // Other
            TokenKind::Eof => "EOF",
            TokenKind::Ignore => "",
        };
        write!(f, "{}", to_write)
    }
}

impl TokenKind {
    pub fn get_keyword(from_str: &str) -> Option<TokenKind> {
        match from_str {
            "and" => Some(TokenKind::And),
            "class" => Some(TokenKind::Class),
            "else" => Some(TokenKind::Else),
            "false" => Some(TokenKind::False),
            "fun" => Some(TokenKind::Fun),
            "for" => Some(TokenKind::For),
            "if" => Some(TokenKind::If),
            "nil" => Some(TokenKind::Nil),
            "or" => Some(TokenKind::Or),
            "print" => Some(TokenKind::Print),
            "return" => Some(TokenKind::Return),
            "super" => Some(TokenKind::Super),
            "this" => Some(TokenKind::This),
            "true" => Some(TokenKind::True),
            "var" => Some(TokenKind::Var),
            "while" => Some(TokenKind::While),
            // otherwise this is not a keyword
            _ => None,
        }
    }
}

pub struct Token {
    kind: TokenKind,
    // the raw characters from the input
    lexeme: String,
    // line and column where the token appears
    position: ScanPosition,
    // length of the token
    length: u64,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, sp: &ScanPosition) -> Self {
        let length = lexeme.len() as u64;

        // scan position is at the end of the scanned token, so adjust that accordingly
        let mut position = sp.clone(); // TODO: this should not .clone() the borrow, just don't borrow
        position.adjust(length);

        Token {
            kind,
            lexeme: lexeme.to_string(),
            position,
            length,
        }
    }

    pub fn can_ignore(&self) -> bool {
        self.kind == TokenKind::Ignore
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}[{}]", self.kind, self.lexeme)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} `{}` {}, len{}",
            self.kind, self.lexeme, self.position, self.length
        )
    }
}
