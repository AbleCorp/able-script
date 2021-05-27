//! Expression evaluator and statement interpreter.
//!
//! To interpret a piece of AbleScript code, you first need to
//! construct an [ExecEnv], which is responsible for storing the stack
//! of local variable and function definitions accessible from an
//! AbleScript snippet. You can then call [ExecEnv::eval_items] to
//! evaluate or execute any number of expressions or statements.

#[deny(missing_docs)]
use std::collections::HashMap;
use std::convert::TryFrom;

use crate::{
    base_55,
    error::{Error, ErrorKind},
    parser::item::{Expr, Iden, Item, Stmt},
    variables::{Value, Variable},
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

        // NOTE(Alex): This is quite nasty, and should probably be
        // re-done using macros or something.
        Ok(match expr {
            Add { left, right } => {
                Int(i32::try_from(self.eval_expr(left)?)? + i32::try_from(self.eval_expr(right)?)?)
            }
            Subtract { left, right } => {
                Int(i32::try_from(self.eval_expr(left)?)? - i32::try_from(self.eval_expr(right)?)?)
            }
            Multiply { left, right } => {
                Int(i32::try_from(self.eval_expr(left)?)? * i32::try_from(self.eval_expr(right)?)?)
            }
            Divide { left, right } => {
                Int(i32::try_from(self.eval_expr(left)?)? / i32::try_from(self.eval_expr(right)?)?)
            }
            Lt { left, right } => {
                Bool(i32::try_from(self.eval_expr(left)?)? < i32::try_from(self.eval_expr(right)?)?)
            }
            Gt { left, right } => {
                Bool(i32::try_from(self.eval_expr(left)?)? > i32::try_from(self.eval_expr(right)?)?)
            }
            Eq { left, right } => Bool(self.eval_expr(left)? == self.eval_expr(right)?),
            Neq { left, right } => Bool(self.eval_expr(left)? != self.eval_expr(right)?),
            And { left, right } => {
                Bool(bool::from(self.eval_expr(left)?) && bool::from(self.eval_expr(right)?))
            }
            Or { left, right } => {
                Bool(bool::from(self.eval_expr(left)?) || bool::from(self.eval_expr(right)?))
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

                // There's always at least one stack frame on the
                // stack if we're evaluating something, so we can
                // `unwrap` here.
                self.stack.iter_mut().last().unwrap().variables.insert(
                    iden.0.clone(),
                    Variable {
                        melo: false,
                        value: init,
                    },
                );
            }
            Stmt::FunctionDeclaration {
                iden: _,
                args: _,
                body: _,
            } => todo!(),
            Stmt::BfFDeclaration { iden: _, body: _ } => todo!(),
            Stmt::If { cond, body } => {
                if self.eval_expr(cond)?.into() {
                    return self.eval_items_hs(body);
                }
            }
            Stmt::FunctionCall { iden: _, args: _ } => todo!(),
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
}
