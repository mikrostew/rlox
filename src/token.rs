use std::fmt;

// keep track of current location
#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    // path to file
    pub file: Option<String>,
    // current line number
    pub line: u64,
    // current column
    pub column: u64,
    // length of token or expression or whatever
    pub length: u64,
}

impl Position {
    pub fn new(file: Option<String>) -> Self {
        Position {
            file,
            // start at the beginning of everything
            line: 1,
            column: 0,
            length: 1, // TODO: start at 0?
        }
    }

    pub fn from_positions(p1: Position, p2: Position) -> Self {
        Position {
            // TODO: for now, assume they are in the same file
            file: p1.file,
            // TODO: and on the same line
            line: p1.line,
            // column starts at p1's column
            column: p1.column,
            length: p2.column - p1.column,
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
        // TODO: implement some kind of wrap-back for multiline stuff?
        if by_chars > 0 && self.column >= (by_chars - 1) {
            self.column -= by_chars - 1;
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

// the kind of token that was scanned
#[derive(PartialEq, Clone)]
pub enum TokenKind {
    // Single-character tokens
    OpenParen,  // '('
    CloseParen, // ')'
    OpenBrace,  // '{'
    CloseBrace, // '}'
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
            TokenKind::OpenParen => "OpenParen",
            TokenKind::CloseParen => "CloseParen",
            TokenKind::OpenBrace => "OpenBrace",
            TokenKind::CloseBrace => "CloseBrace",
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

#[derive(Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    // the raw characters from the input
    pub lexeme: String,
    // where the token appears: file, line, col, length
    pub position: Position,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &str, pos: &Position) -> Self {
        // exceptions for calculating the length
        let length = match kind {
            TokenKind::Eof => 1,
            _ => lexeme.len() as u64,
        };

        // scan position is at the end of the scanned token, so adjust that accordingly
        let mut position = pos.clone();
        position.adjust(length); // TODO: this should set the length of the position

        Token {
            kind,
            lexeme: lexeme.to_string(),
            position,
        }
    }

    pub fn can_ignore(&self) -> bool {
        self.kind == TokenKind::Ignore
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} `{}` {}", self.kind, self.lexeme, self.position)
    }
}
