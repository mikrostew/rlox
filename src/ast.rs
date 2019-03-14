use std::fmt;

// for this grammar:
//
// expression → literal | unary | binary | grouping ;
//
// literal    → NUMBER | STRING | "true" | "false" | "nil" ;
// grouping   → "(" expression ")" ;
// unary      → ( "-" | "!" ) expression ;
// binary     → expression operator expression ;
// operator   → "==" | "!=" | "<" | "<=" | ">" | ">=" | "+"  | "-"  | "*" | "/" ;

// for some ideas on visitor pattern in Rust, see:
// https://github.com/rust-unofficial/patterns/blob/master/patterns/visitor.md

pub enum Expr {
    Binary(Box<Expr>, BinaryOp, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(UnaryOp, Box<Expr>),
}

impl Expr {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>) -> Result<T, String> {
        visitor.visit_expr(self)
    }
}

// so that the matching for this can be restricted
#[derive(PartialEq)]
pub enum UnaryOp {
    Minus,
    Bang,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            UnaryOp::Minus => "-",
            UnaryOp::Bang => "!",
        };
        write!(f, "{}", op)
    }
}

// so that the matching for this can be restricted
pub enum BinaryOp {
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Plus,
    Minus,
    Star,
    Slash,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = match self {
            BinaryOp::BangEqual => "!=",
            BinaryOp::EqualEqual => "==",
            BinaryOp::Greater => ">",
            BinaryOp::GreaterEqual => ">=",
            BinaryOp::Less => "<",
            BinaryOp::LessEqual => "<=",
            BinaryOp::Plus => "+",
            BinaryOp::Minus => "-",
            BinaryOp::Star => "*",
            BinaryOp::Slash => "/",
        };
        write!(f, "{}", op)
    }
}

pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Literal {
    pub fn accept<T>(&self, visitor: &impl Visitor<T>) -> Result<T, String> {
        visitor.visit_literal(self)
    }
}

pub trait Visitor<T> {
    fn visit_expr(&self, e: &Expr) -> Result<T, String>;
    fn visit_literal(&self, l: &Literal) -> Result<T, String>;
}

// actually use the visitor pattern to sort-of pretty-print the AST
pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&self, expr: &Expr) -> Result<String, String> {
        expr.accept(self)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_expr(&self, e: &Expr) -> Result<String, String> {
        Ok(match e {
            Expr::Binary(ref expr1, token, ref expr2) => format!(
                "({} {} {})",
                expr1.accept(self)?,
                token,
                expr2.accept(self)?
            ),
            Expr::Grouping(ref expr) => format!("({})", expr.accept(self)?),
            Expr::Literal(lit) => lit.accept(self)?,
            Expr::Unary(op, ref expr) => format!("({} {})", op, expr.accept(self)?),
        })
    }

    fn visit_literal(&self, l: &Literal) -> Result<String, String> {
        Ok(match l {
            Literal::Bool(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(s) => s.clone(),
        })
    }
}
