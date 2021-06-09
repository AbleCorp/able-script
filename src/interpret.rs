//! Expression evaluator and statement interpreter.
//!
//! To interpret a piece of AbleScript code, you first need to
//! construct an [ExecEnv], which is responsible for storing the stack
//! of local variable and function definitions accessible from an
//! AbleScript snippet. You can then call [ExecEnv::eval_stmts] to
//! evaluate or execute any number of expressions or statements.

#[deny(missing_docs)]
use std::{
    collections::HashMap,
    io::{stdout, Write},
    ops::Range,
    process::exit,
    usize,
};

use rand::random;

use crate::{
    ast::{Expr, Iden, Stmt, StmtKind},
    base_55,
    error::{Error, ErrorKind},
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
    /// We ran out of statements to execute.
    Finished,

    /// A `break` statement occurred at the given span, and was not
    /// caught by a `loop` statement up to this point.
    Break(Range<usize>),

    /// A `hopback` statement occurred at the given span, and was not
    /// caught by a `loop` statement up to this point.
    Hopback(Range<usize>),
}

impl ExecEnv {
    /// Create a new Scope with no predefined variable definitions or
    /// other information.
    pub fn new() -> Self {
        Self {
            // We always need at least one stackframe.
            stack: vec![Default::default()],
        }
    }

    /// Execute a set of Statements in the root stack frame. Return an
    /// error if one or more of the Stmts failed to evaluate, or if a
    /// `break` or `hopback` statement occurred at the top level.
    pub fn eval_stmts(&mut self, stmts: &[Stmt]) -> Result<(), Error> {
        match self.eval_stmts_hs(stmts, false)? {
            HaltStatus::Finished => Ok(()),
            HaltStatus::Break(span) | HaltStatus::Hopback(span) => Err(Error {
                // It's an error to issue a `break` outside of a
                // `loop` statement.
                kind: ErrorKind::TopLevelBreak,
                span: span,
            }),
        }
    }

    /// The same as `eval_stmts`, but report "break" and "hopback"
    /// exit codes as normal conditions in a HaltStatus enum, and
    /// create a new stack frame if `stackframe` is true.
    ///
    /// `interpret`-internal code should typically prefer this
    /// function over `eval_stmts`.
    fn eval_stmts_hs(&mut self, stmts: &[Stmt], stackframe: bool) -> Result<HaltStatus, Error> {
        let init_depth = self.stack.len();

        if stackframe {
            self.stack.push(Default::default());
        }

        let mut final_result = Ok(HaltStatus::Finished);
        for stmt in stmts {
            final_result = self.eval_stmt(stmt);
            if !matches!(final_result, Ok(HaltStatus::Finished)) {
                break;
            }
        }

        if stackframe {
            self.stack.pop();
        }

        // Invariant: stack size must have net 0 change.
        debug_assert_eq!(self.stack.len(), init_depth);
        final_result
    }

    /// Evaluate an Expr, returning its value or an error.
    fn eval_expr(&self, expr: &Expr) -> Result<Value, Error> {
        use crate::ast::BinOpKind::*;
        use crate::ast::ExprKind::*;
        use Value::*;

        Ok(match &expr.kind {
            BinOp { lhs, rhs, kind } => {
                let lhs = self.eval_expr(&lhs)?;
                let rhs = self.eval_expr(&rhs)?;
                match kind {
                    // Arithmetic operators.
                    Add | Subtract | Multiply | Divide => {
                        let lhs = lhs.to_i32(&expr.span)?;
                        let rhs = rhs.to_i32(&expr.span)?;

                        let res = match kind {
                            Add => lhs.checked_add(rhs),
                            Subtract => lhs.checked_sub(rhs),
                            Multiply => lhs.checked_mul(rhs),
                            Divide => lhs.checked_div(rhs),
                            _ => unreachable!(),
                        }
                        .ok_or(Error {
                            kind: ErrorKind::ArithmeticError,
                            span: expr.span.clone(),
                        })?;
                        Int(res)
                    }

                    // Numeric comparisons.
                    Less | Greater => {
                        let lhs = lhs.to_i32(&expr.span)?;
                        let rhs = rhs.to_i32(&expr.span)?;

                        let res = match kind {
                            Less => lhs < rhs,
                            Greater => lhs > rhs,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }

                    // General comparisons.
                    Equal | NotEqual => {
                        let res = match kind {
                            Equal => lhs == rhs,
                            NotEqual => lhs != rhs,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }

                    // Logical connectives.
                    And | Or => {
                        let lhs = lhs.to_bool();
                        let rhs = rhs.to_bool();
                        let res = match kind {
                            And => lhs && rhs,
                            Or => lhs || rhs,
                            _ => unreachable!(),
                        };
                        Bool(res)
                    }
                }
            }
            Not(expr) => Bool(!self.eval_expr(&expr)?.to_bool()),
            Literal(value) => value.clone(),

            // TODO: not too happy with constructing an artificial
            // Iden here.
            Variable(name) => self.get_var(&Iden {
                iden: name.to_owned(),
                span: expr.span.clone(),
            })?,
        })
    }

    /// Perform the action indicated by a statement.
    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<HaltStatus, Error> {
        match &stmt.kind {
            StmtKind::Print(expr) => {
                println!("{}", self.eval_expr(expr)?);
            }
            StmtKind::Var { iden, init } => {
                let init = match init {
                    Some(e) => self.eval_expr(e)?,
                    None => Value::Nul,
                };

                self.decl_var(&iden.iden, init);
            }
            StmtKind::Functio {
                iden: _,
                args: _,
                body: _,
            } => todo!(),
            // This is missing from StmtKind after the interpreter
            // rewrite; presumably, parsing is not yet implemented for
            // it. ~~Alex
            // StmtKind::BfFDeclaration { iden, body } => {
            //     self.decl_var(
            //         &iden.0,
            //         Value::Functio(Functio::BfFunctio(body.as_bytes().into())),
            //     );
            // }
            StmtKind::If { cond, body } => {
                if self.eval_expr(cond)?.to_bool() {
                    return self.eval_stmts_hs(&body.block, true);
                }
            }
            StmtKind::Call { iden, args } => {
                let func = self.get_var(&iden)?;
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
                                        span: stmt.span.clone(),
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
                            kind: ErrorKind::TypeError(iden.iden.to_owned()),
                            span: stmt.span.clone(),
                        })
                    }
                }
            }
            StmtKind::Loop { body } => loop {
                let res = self.eval_stmts_hs(&body.block, true)?;
                match res {
                    HaltStatus::Finished => {}
                    HaltStatus::Break(_) => break,
                    HaltStatus::Hopback(_) => continue,
                }
            },
            // This is missing as well. ~~Alex
            // StmtKind::VarAssignment { iden, value } => {
            //     self.get_var_mut(&iden.0)?.value = self.eval_expr(value)?;
            // }
            StmtKind::Break => {
                return Ok(HaltStatus::Break(stmt.span.clone()));
            }
            StmtKind::HopBack => {
                return Ok(HaltStatus::Hopback(stmt.span.clone()));
            }
            StmtKind::Melo(iden) => {
                self.get_var_mut(&iden)?.melo = true;
            }
            StmtKind::Rlyeh => {
                // Maybe print a creepy error message or something
                // here at some point. ~~Alex
                exit(random());
            }
        }

        Ok(HaltStatus::Finished)
    }

    /// Get the value of a variable. Throw an error if the variable is
    /// inaccessible or banned.
    fn get_var(&self, name: &Iden) -> Result<Value, Error> {
        // One-letter names are reserved as base55 numbers.
        let mut chars = name.iden.chars();
        if let (Some(first), None) = (chars.next(), chars.next()) {
            return Ok(Value::Int(base_55::char2num(first)));
        }

        // Otherwise, search for the name in the stack from top to
        // bottom.
        match self
            .stack
            .iter()
            .rev()
            .find_map(|scope| scope.variables.get(&name.iden))
        {
            Some(var) => {
                if !var.melo {
                    Ok(var.value.clone())
                } else {
                    Err(Error {
                        kind: ErrorKind::MeloVariable(name.iden.to_owned()),
                        span: name.span.clone(),
                    })
                }
            }
            None => Err(Error {
                kind: ErrorKind::UnknownVariable(name.iden.to_owned()),
                span: name.span.clone(),
            }),
        }
    }

    /// Get a mutable reference to a variable. Throw an error if the
    /// variable is inaccessible or banned.
    fn get_var_mut(&mut self, name: &Iden) -> Result<&mut Variable, Error> {
        // This function has a lot of duplicated code with `get_var`,
        // which I feel like is a bad sign...
        match self
            .stack
            .iter_mut()
            .rev()
            .find_map(|scope| scope.variables.get_mut(&name.iden))
        {
            Some(var) => {
                if !var.melo {
                    Ok(var)
                } else {
                    Err(Error {
                        kind: ErrorKind::MeloVariable(name.iden.to_owned()),
                        span: name.span.clone(),
                    })
                }
            }
            None => Err(Error {
                kind: ErrorKind::UnknownVariable(name.iden.to_owned()),
                span: name.span.clone(),
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
    use crate::ast::ExprKind;

    use super::*;

    #[test]
    fn basic_expression_test() {
        // Check that 2 + 2 = 4.
        let env = ExecEnv::new();
        assert_eq!(
            env.eval_expr(&Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(2)),
                        span: 1..1,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(2)),
                        span: 1..1,
                    }),
                    kind: crate::ast::BinOpKind::Add,
                },
                span: 1..1
            })
            .unwrap(),
            Value::Int(4)
        )
    }

    #[test]
    fn type_errors() {
        // The sum of an integer and a boolean results in a type
        // error.
        let env = ExecEnv::new();
        assert!(matches!(
            env.eval_expr(&Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(2)),
                        span: 1..1,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Bool(true)),
                        span: 1..1,
                    }),
                    kind: crate::ast::BinOpKind::Add,
                },
                span: 1..1
            }),
            Err(Error {
                kind: ErrorKind::TypeError(_),
                span: _,
            })
        ));
    }

    #[test]
    fn overflow_should_not_panic() {
        // Integer overflow should throw a recoverable error instead
        // of panicking.
        let env = ExecEnv::new();
        assert!(matches!(
            env.eval_expr(&Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(i32::MAX)),
                        span: 1..1,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(1)),
                        span: 1..1,
                    }),
                    kind: crate::ast::BinOpKind::Add,
                },
                span: 1..1
            }),
            Err(Error {
                kind: ErrorKind::ArithmeticError,
                span: _,
            })
        ));

        // And the same for divide by zero.
        assert!(matches!(
            env.eval_expr(&Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(1)),
                        span: 1..1,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(0)),
                        span: 1..1,
                    }),
                    kind: crate::ast::BinOpKind::Divide,
                },
                span: 1..1
            }),
            Err(Error {
                kind: ErrorKind::ArithmeticError,
                span: _,
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
        env.eval_stmts(&ast).map(|()| Value::Nul)
    }

    #[test]
    #[ignore = "doesn't make sense anymore due to separation of statements & expressions"]
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
    #[ignore = "doesn't make sense anymore due to separation of statements & expressions"]
    fn variable_persistence() {
        // Global variables should persist between invocations of
        // ExecEnv::eval_items().
        let mut env = ExecEnv::new();
        eval(&mut env, "var foo = 32;").unwrap();
        assert_eq!(eval(&mut env, "foo").unwrap(), Value::Int(32));
    }

    #[test]
    #[ignore = "doesn't make sense anymore due to separation of statements & expressions"]
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
