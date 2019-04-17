use std::fmt;
use std::rc::Rc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::ast::{BinaryOp, Expr, Literal, LogicalOp, Stmt, UnaryOp, Visitor};
use crate::environment::Environment;
use crate::token::Position;

// because lox is dynamically typed, we need something to store any possible types
#[derive(PartialEq, Clone)]
pub enum Object {
    Bool(bool),
    Function(LoxFunction),
    NativeFunction(LoxNativeFunction),
    Nil,
    Number(f64),
    String(String),
}

impl Object {
    pub fn is_truthy(&self) -> bool {
        match self {
            // like Ruby, only nil and false are falsey
            Object::Bool(b) => *b,
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

    // TODO: input the position to this, like the functions above
    pub fn call(self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, String> {
        match self {
            Object::Function(f) => f.call(interpreter, args),
            Object::NativeFunction(f) => f.call(interpreter, args),
            _ => Err(format!(
                "[positionTBD] can only call functions and methods, {} is neither",
                self
            )),
        }
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Bool(b) => write!(f, "{}", b),
            Object::Function(fun) => write!(f, "<fn {}>", fun.name),
            Object::Nil => write!(f, "nil"),
            Object::NativeFunction(fun) => write!(f, "<nativefn {}>", fun.name),
            Object::Number(n) => write!(f, "{}", n),
            Object::String(s) => write!(f, "{}", s),
        }
    }
}

// TODO: don't think I need this
// pub trait Callable {
//     fn call(self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, String>;
//     fn arity(self) -> usize;
// }

#[derive(PartialEq, Clone)]
pub struct LoxFunction {
    name: String,
    params: Vec<String>,
    body: Box<Stmt>,
    closure: Rc<Environment>,
}

impl LoxFunction {
    pub fn new(
        name: String,
        params: Vec<String>,
        body: Box<Stmt>,
        closure: &Rc<Environment>,
    ) -> Self {
        LoxFunction {
            name,
            params,
            body,
            closure: Rc::clone(closure),
        }
    }

    fn call(self, interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, String> {
        let env = Environment::new(Some(self.closure));

        // check number of args
        if self.params.len() != args.len() {
            return Err(format!(
                "Expected {} arguments, got {}",
                self.params.len(),
                args.len()
            ));
        }

        for (i, param) in self.params.iter().enumerate() {
            env.define(param, args[i].to_owned());
        }

        interpreter.execute(&self.body, &env)?;

        Ok(Object::Nil)
    }
}

#[derive(PartialEq, Clone)]
pub struct LoxNativeFunction {
    name: String,
    params: Vec<String>,
    function: fn(Vec<Object>) -> Result<Object, String>,
}

impl LoxNativeFunction {
    pub fn new(
        name: String,
        params: Vec<String>,
        function: fn(Vec<Object>) -> Result<Object, String>,
    ) -> Self {
        LoxNativeFunction {
            name,
            params,
            function,
        }
    }

    fn call(self, _interpreter: &mut Interpreter, args: Vec<Object>) -> Result<Object, String> {
        // check number of args
        if self.params.len() != args.len() {
            return Err(format!(
                "Expected {} arguments, got {}",
                self.params.len(),
                args.len()
            ));
        }

        // call the function
        (self.function)(args)
    }
}

pub struct Interpreter {
    // top-level environment
    environment: Rc<Environment>,
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Environment::new(None);

        // global functions

        // clock() - get time since epoch in seconds (with ms after the decimal)
        fn clock_fn(_args: Vec<Object>) -> Result<Object, String> {
            let now = SystemTime::now();
            let time_since_epoch = now
                .duration_since(UNIX_EPOCH)
                .expect("failed to get clock time");
            let time_in_seconds = time_since_epoch.as_millis() as f64 / 1000.0;

            Ok(Object::Number(time_in_seconds))
        }
        let clock = LoxNativeFunction::new("clock".to_string(), Vec::new(), clock_fn);
        globals.define(&"clock".to_string(), Object::NativeFunction(clock));

        Interpreter {
            environment: globals,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), String> {
        for stmt in statements {
            match self.execute(&stmt, &self.environment.clone()) {
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
            Stmt::Function(name, params, ref body) => {
                let lox_function =
                    LoxFunction::new(name.clone(), params.to_owned(), body.to_owned(), env);
                env.define(name, Object::Function(lox_function));
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
            Stmt::While(ref condition, ref body) => {
                while self.evaluate(condition, env)?.is_truthy() {
                    self.execute(body, env)?;
                }
            }
        }
        Ok(Object::Nil)
    }

    fn visit_expr(&mut self, e: &Expr, env: &Rc<Environment>) -> Result<Object, String> {
        match e {
            Expr::Assign(_pos, var_name, ref expr) => {
                // variable assignment
                // evaluate the value and assign to the variable, returning the value
                let value = self.evaluate(expr, env)?;
                env.assign(var_name, value.clone())?;
                Ok(value)
            }
            Expr::Binary(_pos, ref expr1, op, ref expr2) => {
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
            // function/method call
            Expr::Call(_pos, ref callee_expr, args) => {
                // first evaluate the expression for the callee
                // (usually just an identifier, but could be anything)
                let callee = self.evaluate(callee_expr, env)?;

                // then evaluate each of the argument expressions in order,
                // storing the resulting values
                let mut arguments = Vec::new();
                for arg in args {
                    arguments.push(self.evaluate(arg, env)?);
                }

                // call the function
                callee.call(self, arguments)
            }
            // for a group, evaluate the inner expression
            Expr::Grouping(_pos, ref expr) => self.evaluate(expr, env),
            // for a literal, visit the literal
            Expr::Literal(_pos, lit) => lit.accept(self, env),
            Expr::Logical(_pos, ref expr1, op, ref expr2) => {
                // see if we can short-circuit
                let left = self.evaluate(expr1, env)?;
                match op {
                    LogicalOp::Or(_) => {
                        if left.is_truthy() {
                            return Ok(left);
                        }
                    }
                    LogicalOp::And(_) => {
                        if !left.is_truthy() {
                            return Ok(left);
                        }
                    }
                }

                // if we can't short-circuit, return result of right
                self.evaluate(expr2, env)
            }
            Expr::Unary(_pos, op, ref expr) => {
                let right = self.evaluate(expr, env)?;
                Ok(match op {
                    UnaryOp::Minus(pos) => Object::Number(-right.as_number(pos)?),
                    UnaryOp::Bang(_) => Object::Bool(!right.is_truthy()),
                })
            }
            Expr::Variable(_pos, name) => env.get(name),
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
