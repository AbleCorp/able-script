//! Expression evaluator and statement interpreter.
//!
//! To interpret a piece of AbleScript code, you first need to
//! construct an [ExecEnv], which is responsible for storing the stack
//! of local variable and function definitions accessible from an
//! AbleScript snippet. You can then call [ExecEnv::eval_items] to
//! evaluate or execute any number of expressions or statements.

#[deny(missing_docs)]
use std::collections::HashMap;
use std::{
    convert::TryFrom,
    io::{stdout, Write},
};

use crate::{
    base_55,
    error::{Error, ErrorKind},
    parser::item::{Expr, Iden, Item, Stmt},
    variables::{Functio, Value, Variable},
};

/// An environment for executing AbleScript code.
pub struct ExecEnv {
    /// The stack, ordered such that `stack[stack.len() - 1]` is the
    /// top-most (newest) stack frame, and `stack[0]` is the
    /// bottom-most (oldest) stack frame.
    stack: Vec<Scope>,
}

/// A set of visible variable and function definitions in a single
/// stack frame.
#[derive(Default)]
struct Scope {
    /// The mapping from variable names to values.
    variables: HashMap<String, Variable>,
    // In the future, this will store functio definitions and possibly
    // other information.
}

/// The reason a successful series of statements halted.
enum HaltStatus {
    /// The last statement in the list evaluated to this value.
    Value(Value),

    /// A `break` statement occurred and was not caught by a `loop`
    /// statement.
    Break,

    /// A `hopback` statement occurred and was not caught by a `loop`
    /// statement.
    Hopback,
}

impl ExecEnv {
    /// Create a new Scope with no predefined variable definitions or
    /// other information.
    pub fn new() -> Self {
        Self {
            stack: Default::default(),
        }
    }

    /// Evaluate a set of Items in their own stack frame. Return the
    /// value of the last Item evaluated, or an error if one or more
    /// of the Items failed to evaluate or if a `break` or `hopback`
    /// statement occurred at the top level.
    pub fn eval_items(&mut self, items: &[Item]) -> Result<Value, Error> {
        match self.eval_items_hs(items)? {
            HaltStatus::Value(v) => Ok(v),
            HaltStatus::Break | HaltStatus::Hopback => Err(Error {
                // It's an error to issue a `break` outside of a
                // `loop` statement.
                kind: ErrorKind::TopLevelBreak,
                position: 0..0,
            }),
        }
    }

    /// The same as `eval_items`, but report "break" and "hopback"
    /// exit codes as normal conditions in a HaltStatus enum.
    ///
    /// `interpret`-internal code should typically prefer this
    /// function over `eval_items`.
    fn eval_items_hs(&mut self, items: &[Item]) -> Result<HaltStatus, Error> {
        let init_depth = self.stack.len();

        self.stack.push(Default::default());
        let mut final_result = Ok(HaltStatus::Value(Value::Nul));
        for item in items {
            final_result = self.eval_item(item);
            if !matches!(final_result, Ok(HaltStatus::Value(_))) {
                break;
            }
        }
        self.stack.pop();

        // Invariant: stack size must have net 0 change.
        debug_assert_eq!(self.stack.len(), init_depth);
        final_result
    }

    /// Evaluate a single Item, returning its value or an error.
    fn eval_item(&mut self, item: &Item) -> Result<HaltStatus, Error> {
        match item {
            Item::Expr(expr) => self.eval_expr(expr).map(|v| HaltStatus::Value(v)),
            Item::Stmt(stmt) => self.eval_stmt(stmt),
        }
    }

    /// Evaluate an Expr, returning its value or an error.
    fn eval_expr(&self, expr: &Expr) -> Result<Value, Error> {
        use Expr::*;
        use Value::*;

        // NOTE(Alex): This block will get a whole lot cleaner once
        // Ondra's parser stuff gets merged (specifically 97fb19e).
        // For now, though, we've got a bunch of manually-checked
        // unreachable!()s in here which makes me sad...
        Ok(match expr {
            // Binary expressions.
            Add { left, right }
            | Subtract { left, right }
            | Multiply { left, right }
            | Divide { left, right }
            | Lt { left, right }
            | Gt { left, right }
            | Eq { left, right }
            | Neq { left, right }
            | And { left, right }
            | Or { left, right } => {
                let left = self.eval_expr(left)?;
                let right = self.eval_expr(right)?;

                match expr {
                    // Arithmetic operators.
                    Add { .. }
                    | Subtract { .. }
                    | Multiply { .. }
                    | Divide { .. } => {
                        let left = i32::try_from(left)?;
                        let right = i32::try_from(right)?;

                        let res = match expr {
                            Add { .. } => left.checked_add(right),
                            Subtract { .. } => left.checked_sub(right),
                            Multiply { .. } => left.checked_mul(right),
                            Divide { .. } => left.checked_div(right),
                            _ => unreachable!(),
                        }
                        .ok_or(Error {
                            kind: ErrorKind::ArithmeticError,
                            position: 0..0,
                        })?;
                        Int(res)
                    }

                    // Numeric comparisons.
                    Lt { .. } | Gt { .. } => {
                        let left = i32::try_from(left)?;
                        let right = i32::try_from(right)?;

                        let res = match expr {
                            Lt { .. } => left < right,
                            Gt { .. } => left > right,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }

                    // General comparisons.
                    Eq { .. } | Neq { .. } => {
                        let res = match expr {
                            Eq { .. } => left == right,
                            Neq { .. } => left != right,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }

                    // Logical connectives.
                    And { .. } | Or { .. } => {
                        let left = bool::from(left);
                        let right = bool::from(right);
                        let res = match expr {
                            And { .. } => left && right,
                            Or { .. } => left || right,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }

                    // That's all the binary operations.
                    _ => unreachable!(),
                }
            }
            Not(expr) => Bool(!bool::from(self.eval_expr(expr)?)),
            Literal(value) => value.clone(),
            Identifier(Iden(name)) => self.get_var(name)?,
        })
    }

    /// Perform the action indicated by a statement.
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<HaltStatus, Error> {
        match stmt {
            Stmt::Print(expr) => {
                println!("{}", self.eval_expr(expr)?);
            }
            Stmt::VariableDeclaration { iden, init } => {
                let init = match init {
                    Some(e) => self.eval_expr(e)?,
                    None => Value::Nul,
                };

                self.decl_var(&iden.0, init);
            }
            Stmt::FunctionDeclaration {
                iden: _,
                args: _,
                body: _,
            } => todo!(),
            Stmt::BfFDeclaration { iden, body } => {
                self.decl_var(
                    &iden.0,
                    Value::Functio(Functio::BfFunctio(body.as_bytes().into())),
                );
            }
            Stmt::If { cond, body } => {
                if self.eval_expr(cond)?.into() {
                    return self.eval_items_hs(body);
                }
            }
            Stmt::FunctionCall { iden, args } => {
                let func = self.get_var(&iden.0)?;
                match func {
                    Value::Functio(func) => {
                        match func {
                            Functio::BfFunctio(body) => {
                                let mut input: Vec<u8> = vec![];
                                for arg in args {
                                    self.eval_expr(arg)?.bf_write(&mut input);
                                }
                                println!("input = {:?}", input);
                                let mut output = vec![];

                                crate::brian::interpret_with_io(&body, &input as &[_], &mut output)
                                    .map_err(|e| Error {
                                        kind: ErrorKind::BfInterpretError(e),
                                        position: 0..0,
                                    })?;

                                // I guess Brainfuck functions write
                                // output to stdout? It's not quite
                                // clear to me what else to do. ~~Alex
                                stdout()
                                    .write_all(&output)
                                    .expect("Failed to write to stdout");
                            }
                            Functio::AbleFunctio(_) => {
                                todo!()
                            }
                        }
                    }
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::TypeError(iden.0.to_owned()),
                            position: 0..0,
                        })
                    }
                }
            }
            Stmt::Loop { body } => loop {
                let res = self.eval_items_hs(body)?;
                match res {
                    HaltStatus::Value(_) => {}
                    HaltStatus::Break => break,
                    HaltStatus::Hopback => continue,
                }
            },
            Stmt::VarAssignment { iden, value } => {
                self.get_var_mut(&iden.0)?.value = self.eval_expr(value)?;
            }
            Stmt::Break => {
                return Ok(HaltStatus::Break);
            }
            Stmt::HopBack => {
                return Ok(HaltStatus::Hopback);
            }
            Stmt::Melo(iden) => {
                self.get_var_mut(&iden.0)?.melo = true;
            }
        }

        Ok(HaltStatus::Value(Value::Nul))
    }

    /// Get the value of a variable. Throw an error if the variable is
    /// inaccessible or banned.
    fn get_var(&self, name: &str) -> Result<Value, Error> {
        // One-letter names are reserved as base55 numbers.
        let mut chars = name.chars();
        if let (Some(first), None) = (chars.next(), chars.next()) {
            return Ok(Value::Int(base_55::char2num(first)));
        }

        // Otherwise, search for the name in the stack from top to
        // bottom.
        match self
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.variables.get(name))
        {
            Some(var) => {
                if !var.melo {
                    Ok(var.value.clone())
                } else {
                    Err(Error {
                        kind: ErrorKind::MeloVariable(name.to_owned()),
                        // TODO: figure out some way to avoid this
                        // 0..0 dumbness
                        position: 0..0,
                    })
                }
            }
            None => Err(Error {
                kind: ErrorKind::UnknownVariable(name.to_owned()),
                position: 0..0,
            }),
        }
    }

    /// Get a mutable reference to a variable. Throw an error if the
    /// variable is inaccessible or banned.
    fn get_var_mut(&mut self, name: &str) -> Result<&mut Variable, Error> {
        // This function has a lot of duplicated code with `get_var`,
        // which I feel like is a bad sign...
        match self
            .stack
            .iter_mut()
            .rev()
            .find_map(|scope| scope.variables.get_mut(name))
        {
            Some(var) => {
                if !var.melo {
                    Ok(var)
                } else {
                    Err(Error {
                        kind: ErrorKind::MeloVariable(name.to_owned()),
                        position: 0..0,
                    })
                }
            }
            None => Err(Error {
                kind: ErrorKind::UnknownVariable(name.to_owned()),
                position: 0..0,
            }),
        }
    }

    /// Declares a new variable, with the given initial value.
    fn decl_var(&mut self, name: &str, value: Value) {
        self.stack
            .iter_mut()
            .last()
            .expect("Declaring variable on empty stack")
            .variables
            .insert(name.to_owned(), Variable { melo: false, value });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic_expression_test() {
        // Check that 2 + 2 = 4.
        let mut env = ExecEnv::new();
        assert_eq!(
            env.eval_items(&[Item::Expr(Expr::Add {
                left: Box::new(Expr::Literal(Value::Int(2))),
                right: Box::new(Expr::Literal(Value::Int(2))),
            })])
            .unwrap(),
            Value::Int(4)
        )
    }

    #[test]
    fn type_errors() {
        // The sum of an integer and a boolean results in a type
        // error.
        let mut env = ExecEnv::new();
        assert!(matches!(
            env.eval_items(&[Item::Expr(Expr::Add {
                left: Box::new(Expr::Literal(Value::Int(i32::MAX))),
                right: Box::new(Expr::Literal(Value::Bool(false))),
            })]),
            Err(Error {
                kind: ErrorKind::TypeError(_),
                position: _,
            })
        ));
    }

    #[test]
    fn overflow_should_not_panic() {
        // Integer overflow should throw a recoverable error instead
        // of panicking.
        let mut env = ExecEnv::new();
        assert!(matches!(
            env.eval_items(&[Item::Expr(Expr::Add {
                left: Box::new(Expr::Literal(Value::Int(i32::MAX))),
                right: Box::new(Expr::Literal(Value::Int(1))),
            })]),
            Err(Error {
                kind: ErrorKind::ArithmeticError,
                position: _,
            })
        ));

        // And the same for divide by zero.
        assert!(matches!(
            env.eval_items(&[Item::Expr(Expr::Divide {
                left: Box::new(Expr::Literal(Value::Int(1))),
                right: Box::new(Expr::Literal(Value::Int(0))),
            })]),
            Err(Error {
                kind: ErrorKind::ArithmeticError,
                position: _,
            })
        ));
    }

    // From here on out, I'll use this function to parse and run
    // expressions, because writing out abstract syntax trees by hand
    // takes forever and is error-prone.
    fn eval(env: &mut ExecEnv, src: &str) -> Result<Value, Error> {
        let mut parser = crate::parser::Parser::new(src);

        // We can assume there won't be any syntax errors in the
        // interpreter tests.
        let ast = parser.init().unwrap();
        env.eval_items(&ast)
    }

    #[test]
    fn variable_decl_and_assignment() {
        // Declaring and reading from a variable.
        assert_eq!(
            eval(&mut ExecEnv::new(), "var foo = 32; foo + 1").unwrap(),
            Value::Int(33)
        );

        // It should be possible to overwrite variables as well.
        assert_eq!(
            eval(&mut ExecEnv::new(), "var bar = 10; bar = 20; bar").unwrap(),
            Value::Int(20)
        );

        // But variable assignment should be illegal when the variable
        // hasn't been declared in advance.
        eval(&mut ExecEnv::new(), "baz = 10;").unwrap_err();
    }

    #[test]
    fn variable_persistence() {
        // Global variables should persist between invocations of
        // ExecEnv::eval_items().
        let mut env = ExecEnv::new();
        eval(&mut env, "var foo = 32;").unwrap();
        assert_eq!(eval(&mut env, "foo").unwrap(), Value::Int(32));
    }

    #[test]
    fn scope_visibility_rules() {
        // Declaration and assignment of variables declared in an `if`
        // statement should have no effect on those declared outside
        // of it.
        assert_eq!(
            eval(
                &mut ExecEnv::new(),
                "var foo = 1; if (true) { var foo = 2; foo = 3; } foo"
            )
            .unwrap(),
            Value::Int(1)
        );
    }
}
