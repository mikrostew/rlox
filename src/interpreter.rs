use crate::ast::{BinaryOp, Expr, Literal, UnaryOp, Visitor};

// because lox is dynamically typed, we need something to store any possible types
#[derive(PartialEq)]
enum Object {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
}

impl Object {
    pub fn is_truthy(self) -> bool {
        match self {
            // like Ruby, only nil and false are falsey
            Object::Bool(b) => b,
            Object::Nil => false,
            // anything else is truthy
            _ => true,
        }
    }

    // TODO: return Result
    pub fn as_number(self) -> f64 {
        1.23
    }

    // TODO: return Result
    pub fn as_string(self) -> String {
        "1.23".to_string()
    }
}

struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    // TODO: this will probably need to return Result<>
    pub fn evaluate(&self, expr: &Box<Expr>) -> Object {
        expr.accept(self)
    }
}

fn add_or_concat(left: Object, right: Object) -> Object {
    // decide based on the left-hand operand
    match left {
        Object::Number(n) => Object::Number(n + right.as_number()),
        Object::String(s) => Object::String(s + &right.as_string()),
        // TODO: anything else is an Err()
        _ => Object::Nil,
    }
}

impl Visitor<Object> for Interpreter {
    fn visit_expr(&self, e: &Expr) -> Object {
        match e {
            Expr::Binary(ref expr1, op, ref expr2) => {
                let left = self.evaluate(expr1);
                let right = self.evaluate(expr2);

                match op {
                    // equality
                    BinaryOp::BangEqual => Object::Bool(left != right),
                    BinaryOp::EqualEqual => Object::Bool(left == right),
                    // comparison
                    BinaryOp::Greater => Object::Bool(left.as_number() > right.as_number()),
                    BinaryOp::GreaterEqual => Object::Bool(left.as_number() >= right.as_number()),
                    BinaryOp::Less => Object::Bool(left.as_number() < right.as_number()),
                    BinaryOp::LessEqual => Object::Bool(left.as_number() <= right.as_number()),
                    // arithmetic or concat
                    BinaryOp::Plus => add_or_concat(left, right),
                    // arithmetic
                    BinaryOp::Minus => Object::Number(left.as_number() - right.as_number()),
                    BinaryOp::Star => Object::Number(left.as_number() * right.as_number()),
                    BinaryOp::Slash => Object::Number(left.as_number() / right.as_number()),
                }
            }
            // for a group, evaluate the inner expression
            Expr::Grouping(ref expr) => self.evaluate(expr),
            // for a literal, visit the literal
            Expr::Literal(lit) => lit.accept(self),
            Expr::Unary(op, ref expr) => {
                let right = self.evaluate(expr);
                match op {
                    UnaryOp::Minus => Object::Number(-right.as_number()),
                    UnaryOp::Bang => Object::Bool(!right.is_truthy()),
                }
            }
        }
    }

    fn visit_literal(&self, l: &Literal) -> Object {
        match l {
            // just convert the literal to an object
            Literal::Bool(b) => Object::Bool(*b),
            Literal::Nil => Object::Nil,
            Literal::Number(n) => Object::Number(*n),
            Literal::String(s) => Object::String(s.to_string()),
        }
    }
}
