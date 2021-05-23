//! Expression evaluator and statement interpreter.
//!
//! To interpret a piece of AbleScript code, you first need to
//! construct a [Scope], which is responsible for storing the list of
//! variable and function definitions accessible from an AbleScript
//! snippet. You can then call [Scope::eval_items] to evaluate or
//! execute any number of expressions or statements.

#[deny(missing_docs)]
use std::collections::HashMap;
use std::convert::TryFrom;

use crate::{
    error::{Error, ErrorKind},
    parser::item::{Expr, Iden, Item, Stmt},
    variables::{Value, Variable},
};

/// A set of visible variable and function definitions, which serves
/// as a context in which expressions can be evaluated.
pub struct Scope {
    /// The mapping from variable names to values.
    variables: HashMap<String, Variable>,
    // In the future, this will store functio definitions, a link to a
    // parent scope (so we can have nested scopes), and possibly other
    // information.
}

impl Scope {
    /// Create a new Scope with no predefined variable definitions or
    /// other information.
    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    /// Evaluate a set of Items. Returns the value of the last Item
    /// evaluated, or an error if one or more of the Items failed to
    /// evaluate.
    pub fn eval_items(&mut self, items: &[Item]) -> Result<Value, Error> {
        items
            .iter()
            .map(|item| self.eval_item(item))
            .try_fold(Value::Nul, |_, result| result)
    }

    /// Evaluate a single Item, returning its value or an error.
    fn eval_item(&mut self, item: &Item) -> Result<Value, Error> {
        match item {
            Item::Expr(expr) => self.eval_expr(expr),
            Item::Stmt(stmt) => self.eval_stmt(stmt).map(|_| Value::Nul),
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
            Identifier(Iden(name)) => self
                .variables
                .get(name)
                .ok_or_else(|| Error {
                    kind: ErrorKind::UnknownVariable(name.to_owned()),
                    // TODO: figure out some way to avoid this 0..0
                    // dumbness
                    position: 0..0,
                })
                .and_then(|var| {
                    if !var.melo {
                        Ok(var.value.clone())
                    } else {
                        Err(Error {
                            kind: ErrorKind::MeloVariable(name.to_owned()),
                            position: 0..0,
                        })
                    }
                })?,
        })
    }

    /// Perform the action indicated by a statement.
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<(), Error> {
        match stmt {
            Stmt::Print(expr) => {
                println!("{}", self.eval_expr(expr)?);
            }
            Stmt::VariableDeclaration { iden, init } => {
                self.variables.insert(
                    iden.0.clone(),
                    Variable {
                        melo: false,
                        value: match init {
                            Some(init) => self.eval_expr(init)?,
                            None => Value::Nul,
                        },
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
                    self.eval_items(body)?;
                }
            }
            Stmt::FunctionCall { iden: _, args: _ } => todo!(),
            Stmt::Loop { body } => {
                loop {
                    // For now, loops run forever until they reach an
                    // error.
                    self.eval_items(body)?;
                }
            }
            Stmt::VarAssignment { iden, value } => {
                let value = self.eval_expr(value)?;
                let record = self.variables.get_mut(&iden.0).ok_or_else(|| Error {
                    kind: ErrorKind::UnknownVariable(iden.0.clone()),
                    position: 0..0,
                })?;

                if record.melo {
                    return Err(Error {
                        kind: ErrorKind::MeloVariable(iden.0.clone()),
                        position: 0..0,
                    });
                }

                record.value = value;
            }
            Stmt::Break => todo!(),
            Stmt::HopBack => todo!(),
            Stmt::Melo(iden) => {
                let record = self.variables.get_mut(&iden.0).ok_or_else(|| Error {
                    kind: ErrorKind::UnknownVariable(iden.0.clone()),
                    position: 0..0,
                })?;

                record.melo = true;
            }
        }

        Ok(())
    }
}
