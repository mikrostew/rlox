use std::fmt;
use std::rc::Rc;

use crate::ast::{BinaryOp, Expr, Literal, Stmt, UnaryOp, Visitor};
use crate::environment::Environment;
use crate::token::Position;

// because lox is dynamically typed, we need something to store any possible types
#[derive(PartialEq, Clone)]
pub enum Object {
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

    pub fn as_number(self, pos: &Position) -> Result<f64, String> {
        match self {
            Object::Number(n) => Ok(n),
            _ => Err(format!("[{}] operand must be a number, got {}", pos, self)),
        }
    }

    pub fn as_string(self, pos: &Position) -> Result<String, String> {
        match self {
            Object::String(s) => Ok(s),
            _ => Err(format!("[{}] operand must be a string, got {}", pos, self)),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Bool(b) => write!(f, "{}", b),
            Object::Nil => write!(f, "nil"),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
        }
    }
}

pub struct Interpreter;

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {}
    }

    pub fn interpret(
        &mut self,
        statements: Vec<Stmt>,
        env: &Rc<Environment>,
    ) -> Result<(), String> {
        for stmt in statements {
            match self.execute(&stmt, env) {
                Ok(_) => (),
                Err(e) => {
                    eprintln!("Runtime Error:");
                    eprintln!("{}", e);
                }
            }
        }
        Ok(())
    }

    fn evaluate(&mut self, expr: &Box<Expr>, env: &Rc<Environment>) -> Result<Object, String> {
        expr.accept(self, env)
    }

    fn execute(&mut self, stmt: &Stmt, env: &Rc<Environment>) -> Result<Object, String> {
        stmt.accept(self, env)
    }

    fn exec_block(&mut self, stmts: &Vec<Stmt>, env: &Rc<Environment>) -> Result<(), String> {
        for stmt in stmts {
            self.execute(stmt, env)?;
        }
        Ok(())
    }
}

fn add_or_concat(left: Object, right: Object, pos: &Position) -> Result<Object, String> {
    // decide based on the left-hand operand
    Ok(match left {
        Object::Number(n) => Object::Number(n + right.as_number(pos)?),
        Object::String(s) => Object::String(s + &right.as_string(pos)?),
        _ => {
            return Err(format!(
                "[{}] operands to `+` must be numbers or strings",
                pos
            ));
        }
    })
}

impl Visitor<Object> for Interpreter {
    fn visit_stmt<'a>(&mut self, stmt: &Stmt, env: &Rc<Environment>) -> Result<Object, String> {
        match stmt {
            Stmt::Block(statements) => {
                // new environment for this block
                let block_env = Environment::new(Some(env.clone()));
                self.exec_block(statements, &block_env)?;
            }
            Stmt::Expression(ref expr) => {
                self.evaluate(expr, env)?;
            }
            Stmt::If(ref if_expr, ref then_stmt, ref opt_else_stmt) => {
                let condition = self.evaluate(if_expr, env)?;
                if condition.is_truthy() {
                    self.execute(then_stmt, env)?;
                } else {
                    // only execute the else_stmt if there is one
                    if let Some(else_stmt) = opt_else_stmt {
                        self.execute(else_stmt, env)?;
                    }
                }
            }
            Stmt::Print(ref expr) => {
                let value = self.evaluate(expr, env)?;
                println!("{}", value);
            }
            Stmt::Var(name, ref expr) => {
                // variable declaration
                // evaluate the value and assign to the new variable
                let value = self.evaluate(expr, env)?;
                env.define(name, value);
            }
        }
        Ok(Object::Nil)
    }

    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<Object, String> {
        match e {
            Expr::Assign(var_name, ref expr) => {
                // variable assignment
                // evaluate the value and assign to the variable, returning the value
                let value = self.evaluate(expr, env)?;
                env.assign(var_name, value.clone())?;
                Ok(value)
            }
            Expr::Binary(ref expr1, op, ref expr2) => {
                let left = self.evaluate(expr1, env)?;
                let right = self.evaluate(expr2, env)?;

                Ok(match op {
                    // equality
                    BinaryOp::BangEqual(_) => Object::Bool(left != right),
                    BinaryOp::EqualEqual(_) => Object::Bool(left == right),
                    // comparison
                    BinaryOp::Greater(pos) => {
                        Object::Bool(left.as_number(pos)? > right.as_number(pos)?)
                    }
                    BinaryOp::GreaterEqual(pos) => {
                        Object::Bool(left.as_number(pos)? >= right.as_number(pos)?)
                    }
                    BinaryOp::Less(pos) => {
                        Object::Bool(left.as_number(pos)? < right.as_number(pos)?)
                    }
                    BinaryOp::LessEqual(pos) => {
                        Object::Bool(left.as_number(pos)? <= right.as_number(pos)?)
                    }
                    // arithmetic or concat
                    BinaryOp::Plus(pos) => add_or_concat(left, right, pos)?,
                    // arithmetic
                    BinaryOp::Minus(pos) => {
                        Object::Number(left.as_number(pos)? - right.as_number(pos)?)
                    }
                    BinaryOp::Star(pos) => {
                        Object::Number(left.as_number(pos)? * right.as_number(pos)?)
                    }
                    BinaryOp::Slash(pos) => {
                        Object::Number(left.as_number(pos)? / right.as_number(pos)?)
                    }
                })
            }
            // for a group, evaluate the inner expression
            Expr::Grouping(ref expr) => self.evaluate(expr, env),
            // for a literal, visit the literal
            Expr::Literal(lit) => lit.accept(self, env),
            Expr::Unary(op, ref expr) => {
                let right = self.evaluate(expr, env)?;
                Ok(match op {
                    UnaryOp::Minus(pos) => Object::Number(-right.as_number(pos)?),
                    UnaryOp::Bang(_) => Object::Bool(!right.is_truthy()),
                })
            }
            Expr::Variable(name) => env.get(name),
        }
    }

    fn visit_literal(&self, l: &Literal, _env: &Rc<Environment>) -> Result<Object, String> {
        Ok(match l {
            // just convert the literal to an object
            Literal::Bool(b) => Object::Bool(*b),
            Literal::Nil => Object::Nil,
            Literal::Number(n) => Object::Number(*n),
            Literal::String(s) => Object::String(s.to_string()),
        })
    }
}
