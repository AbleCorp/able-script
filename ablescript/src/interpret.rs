//! Expression evaluator and statement interpreter.
//!
//! To interpret a piece of AbleScript code, you first need to
//! construct an [ExecEnv], which is responsible for storing the stack
//! of local variable and function definitions accessible from an
//! AbleScript snippet. You can then call [ExecEnv::eval_stmts] to
//! evaluate or execute any number of expressions or statements.

#![deny(missing_docs)]
use std::{
    cell::RefCell,
    collections::{HashMap, VecDeque},
    io::{stdin, stdout, Read, Write},
    ops::Range,
    process::exit,
    rc::Rc,
};

use rand::random;

use crate::{
    ast::{Expr, ExprKind, Iden, Stmt, StmtKind},
    base_55,
    consts::{self, ablescript_consts},
    error::{Error, ErrorKind},
    variables::{Functio, Value, Variable},
};

/// An environment for executing AbleScript code.
pub struct ExecEnv {
    /// The stack, ordered such that `stack[stack.len() - 1]` is the
    /// top-most (newest) stack frame, and `stack[0]` is the
    /// bottom-most (oldest) stack frame.
    stack: Vec<Scope>,

    /// The `read` statement maintains a buffer of up to 7 bits,
    /// because input comes from the operating system 8 bits at a time
    /// (via stdin) but gets delivered to AbleScript 3 bits at a time
    /// (via the `read` statement). We store each of those bits as
    /// booleans to facilitate easy manipulation.
    read_buf: VecDeque<bool>,
}

/// A set of visible variable and function definitions in a single
/// stack frame.
struct Scope {
    /// The mapping from variable names to values.
    variables: HashMap<String, Variable>,
}

impl Default for Scope {
    fn default() -> Self {
        Self {
            variables: ablescript_consts(),
        }
    }
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

/// The number of bits the `read` statement reads at once from
/// standard input.
pub const READ_BITS: u8 = 3;

impl ExecEnv {
    /// Create a new Scope with no predefined variable definitions or
    /// other information.
    pub fn new() -> Self {
        Self {
            // We always need at least one stackframe.
            stack: vec![Default::default()],
            read_buf: Default::default(),
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
                span,
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
                let lhs = self.eval_expr(lhs)?;
                let rhs = self.eval_expr(rhs)?;
                match kind {
                    Add => lhs + rhs,
                    Subtract => lhs - rhs,
                    Multiply => lhs * rhs,
                    Divide => lhs / rhs,
                    Greater => Value::Bool(lhs > rhs),
                    Less => Value::Bool(lhs < rhs),
                    Equal => Value::Bool(lhs == rhs),
                    NotEqual => Value::Bool(lhs != rhs),
                }
            }
            Not(expr) => !self.eval_expr(expr)?,
            Literal(value) => value.clone(),
            ExprKind::Cart(members) => Value::Cart(
                members
                    .iter()
                    .map(|(value, key)| {
                        self.eval_expr(value).and_then(|value| {
                            self.eval_expr(key)
                                .map(|key| (key, Rc::new(RefCell::new(value))))
                        })
                    })
                    .collect::<Result<HashMap<_, _>, _>>()?,
            ),
            Index { expr, index } => {
                let value = self.eval_expr(expr)?;
                let index = self.eval_expr(index)?;

                value
                    .into_cart()
                    .get(&index)
                    .map(|x| x.borrow().clone())
                    .unwrap_or(Value::Nul)
            }

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
            StmtKind::Functio { iden, params, body } => {
                self.decl_var(
                    &iden.iden,
                    Value::Functio(Functio::AbleFunctio {
                        params: params.iter().map(|iden| iden.iden.to_string()).collect(),
                        body: body.block.to_owned(),
                    }),
                );
            }
            StmtKind::BfFunctio {
                iden,
                tape_len,
                code,
            } => {
                self.decl_var(
                    &iden.iden,
                    Value::Functio(Functio::BfFunctio {
                        instructions: code.to_owned(),
                        tape_len: tape_len
                            .as_ref()
                            .map(|tape_len| self.eval_expr(tape_len).map(|v| v.into_i32() as usize))
                            .unwrap_or(Ok(crate::brian::DEFAULT_TAPE_SIZE_LIMIT))?,
                    }),
                );
            }
            StmtKind::If { cond, body } => {
                if self.eval_expr(cond)?.into_bool() {
                    return self.eval_stmts_hs(&body.block, true);
                }
            }
            StmtKind::Call { expr, args } => {
                let func = self.eval_expr(expr)?.into_functio();

                self.fn_call(func, args, &stmt.span)?;
            }
            StmtKind::Loop { body } => loop {
                let res = self.eval_stmts_hs(&body.block, true)?;
                match res {
                    HaltStatus::Finished => {}
                    HaltStatus::Break(_) => break,
                    HaltStatus::Hopback(_) => continue,
                }
            },
            StmtKind::Assign { iden, value } => {
                let value = self.eval_expr(value)?;
                self.get_var_mut(iden)?.value.replace(value);
            }
            StmtKind::Break => {
                return Ok(HaltStatus::Break(stmt.span.clone()));
            }
            StmtKind::HopBack => {
                return Ok(HaltStatus::Hopback(stmt.span.clone()));
            }
            StmtKind::Melo(iden) => {
                self.get_var_mut(iden)?.melo = true;
            }
            StmtKind::Rlyeh => {
                // Maybe print a creepy error message or something
                // here at some point. ~~Alex
                exit(random());
            }
            StmtKind::Rickroll => {
                stdout()
                    .write_all(include_str!("rickroll").as_bytes())
                    .expect("Failed to write to stdout");
            }
            StmtKind::Read(iden) => {
                let mut value = 0;
                for _ in 0..READ_BITS {
                    value <<= 1;
                    value += self.get_bit()? as i32;
                }

                self.get_var_mut(iden)?.value.replace(Value::Int(value));
            }
        }

        Ok(HaltStatus::Finished)
    }

    /// Call a function with the given arguments (i.e., actual
    /// parameters). If the function invocation fails for some reason,
    /// report the error at `span`.
    fn fn_call(&mut self, func: Functio, args: &[Expr], span: &Range<usize>) -> Result<(), Error> {
        // Arguments that are ExprKind::Variable are pass by
        // reference; all other expressions are pass by value.
        let args = args
            .iter()
            .map(|arg| {
                if let ExprKind::Variable(name) = &arg.kind {
                    self.get_var_rc(&Iden {
                        iden: name.to_owned(),
                        span: arg.span.clone(),
                    })
                } else {
                    self.eval_expr(arg).map(|v| Rc::new(RefCell::new(v)))
                }
            })
            .collect::<Result<Vec<_>, Error>>()?;

        match func {
            Functio::BfFunctio {
                instructions,
                tape_len,
            } => {
                let mut input: Vec<u8> = vec![];
                for arg in args {
                    arg.borrow().bf_write(&mut input);
                }

                let mut output = vec![];

                crate::brian::Interpreter::from_ascii_with_tape_limit(
                    &instructions,
                    &input as &[_],
                    tape_len,
                )
                .interpret_with_output(&mut output)
                .map_err(|e| Error {
                    kind: ErrorKind::BfInterpretError(e),
                    span: span.to_owned(),
                })?;

                stdout()
                    .write_all(&output)
                    .expect("Failed to write to stdout");
            }
            Functio::AbleFunctio { params, body } => {
                if params.len() != args.len() {
                    return Err(Error {
                        kind: ErrorKind::MismatchedArgumentError,
                        span: span.to_owned(),
                    });
                }

                self.stack.push(Default::default());

                for (param, arg) in params.iter().zip(args.iter()) {
                    self.decl_var_shared(param, arg.to_owned());
                }

                let res = self.eval_stmts_hs(&body, false);

                self.stack.pop();
                res?;
            }
            Functio::Eval(code) => {
                if args.len() != 0 {
                    return Err(Error {
                        kind: ErrorKind::MismatchedArgumentError,
                        span: span.to_owned(),
                    });
                }

                let stmts = crate::parser::Parser::new(&code).init()?;
                self.eval_stmts(&stmts)?;
            }
        }
        Ok(())
    }

    /// Get a single bit from the bit buffer, or refill it from
    /// standard input if it is empty.
    fn get_bit(&mut self) -> Result<bool, Error> {
        const BITS_PER_BYTE: u8 = 8;

        if self.read_buf.is_empty() {
            let mut data = [0];
            stdin().read_exact(&mut data)?;

            for n in (0..BITS_PER_BYTE).rev() {
                self.read_buf.push_back(((data[0] >> n) & 1) != 0);
            }
        }

        Ok(self
            .read_buf
            .pop_front()
            .expect("We just pushed to the buffer if it was empty"))
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
                    Ok(var.value.borrow().clone())
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

    /// Get an Rc'd pointer to the value of a variable. Throw an error
    /// if the variable is inaccessible or banned.
    fn get_var_rc(&mut self, name: &Iden) -> Result<Rc<RefCell<Value>>, Error> {
        Ok(self.get_var_mut(name)?.value.clone())
    }

    /// Declare a new variable, with the given initial value.
    fn decl_var(&mut self, name: &str, value: Value) {
        self.decl_var_shared(name, Rc::new(RefCell::new(value)));
    }

    /// Declare a new variable, with the given shared initial value.
    fn decl_var_shared(&mut self, name: &str, value: Rc<RefCell<Value>>) {
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
    fn type_coercions() {
        // The sum of an integer and a boolean causes a boolean
        // coercion.
        let env = ExecEnv::new();
        assert_eq!(
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
            })
            .unwrap(),
            Value::Int(3)
        );
    }

    #[test]
    fn overflow_should_not_panic() {
        // Integer overflow should throw a recoverable error instead
        // of panicking.
        let env = ExecEnv::new();
        assert_eq!(
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
            })
            .unwrap(),
            Value::Int(-2147483648)
        );

        // And the same for divide by zero.
        assert_eq!(
            env.eval_expr(&Expr {
                kind: ExprKind::BinOp {
                    lhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(84)),
                        span: 1..1,
                    }),
                    rhs: Box::new(Expr {
                        kind: ExprKind::Literal(Value::Int(0)),
                        span: 1..1,
                    }),
                    kind: crate::ast::BinOpKind::Divide,
                },
                span: 1..1
            })
            .unwrap(),
            Value::Int(2)
        );
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
    fn variable_decl_and_assignment() {
        // Functions have no return values, so use some
        // pass-by-reference hacks to detect the correct
        // functionality.
        let mut env = ExecEnv::new();

        // Declaring and reading from a variable.
        eval(&mut env, "var foo = 32; var bar = foo + 1;").unwrap();
        assert_eq!(
            env.get_var(&Iden {
                iden: "bar".to_owned(),
                span: 1..1,
            })
            .unwrap(),
            Value::Int(33)
        );

        // Assigning an existing variable.
        eval(&mut env, "foo = \"hi\";").unwrap();
        assert_eq!(
            env.get_var(&Iden {
                iden: "foo".to_owned(),
                span: 1..1,
            })
            .unwrap(),
            Value::Str("hi".to_owned())
        );

        // But variable assignment should be illegal when the variable
        // hasn't been declared in advance.
        eval(&mut env, "invalid = bar + 1;").unwrap_err();
    }

    #[test]
    fn scope_visibility_rules() {
        // Declaration and assignment of variables declared in an `if`
        // statement should have no effect on those declared outside
        // of it.
        let mut env = ExecEnv::new();
        eval(
            &mut env,
            "var foo = 1; foo = 2; if (true) { var foo = 3; foo = 4; }",
        )
        .unwrap();

        assert_eq!(
            env.get_var(&Iden {
                iden: "foo".to_owned(),
                span: 1..1,
            })
            .unwrap(),
            Value::Int(2)
        );
    }
}
