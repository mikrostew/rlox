use crate::token::Token;

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
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
    Unary(Token, Box<Expr>),
}

impl Expr {
    fn accept<T>(&self, visitor: &impl Visitor<T>) -> T {
        visitor.visit_expr(self)
    }
}

pub enum Literal {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Literal {
    fn accept<T>(&self, visitor: &impl Visitor<T>) -> T {
        visitor.visit_literal(self)
    }
}

pub trait Visitor<T> {
    fn visit_expr(&self, e: &Expr) -> T;
    fn visit_literal(&self, l: &Literal) -> T;
}

// actually use the visitor pattern to sort-of pretty-print the AST
pub struct AstPrinter;

impl AstPrinter {
    pub fn new() -> Self {
        AstPrinter {}
    }

    pub fn print(&self, expr: Expr) -> String {
        expr.accept(self)
    }
}

impl Visitor<String> for AstPrinter {
    fn visit_expr(&self, e: &Expr) -> String {
        match e {
            Expr::Binary(ref expr1, token, ref expr2) => {
                format!("({} {} {})", expr1.accept(self), token, expr2.accept(self))
            }
            Expr::Grouping(ref expr) => format!("({})", expr.accept(self)),
            Expr::Literal(lit) => lit.accept(self),
            Expr::Unary(token, ref expr) => format!("({} {})", token, expr.accept(self)),
        }
    }

    fn visit_literal(&self, l: &Literal) -> String {
        match l {
            Literal::Bool(b) => b.to_string(),
            Literal::Nil => "nil".to_string(),
            Literal::Number(num) => format!("{}", num),
            Literal::String(s) => s.clone(),
        }
    }
}
