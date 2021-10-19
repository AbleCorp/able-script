//! A brainfuck interpreter capable of executing arbitrary code, with arbitrary inputs and outputs.
//!
//! If you just want to execute some simple brainfuck, check the [`interpret_with_io`] function.
//!
//! To construct the interpreter, use the [`from_ascii`] or [`from_ascii_with_input_buffer`] methods
//! (or their variants that take a maximum tape size). The latter grants access to
//! the method [`add_input`], which allows for the addition of input while the interpreter is running.
//!
//! [`from_ascii`]: Interpreter::from_ascii
//! [`from_ascii_with_input_buffer`]: Interpreter::from_ascii_with_input_buffer
//! [`add_input`]: Interpreter::add_input
//!
//! Finally, to run the interpreter, you can use the [`advance`], [`advance_until_io`], or [`interpret_with_output`] methods.
//!
//! [`advance`]: Interpreter::advance
//! [`advance_until_io`]: Interpreter::advance_until_io
//! [`interpret_with_output`]: Interpreter::interpret_with_output

#![deny(missing_docs)]

use std::{
    collections::VecDeque,
    error::Error,
    fmt::Display,
    io::{Read, Write},
};

// NOTE(Able): This is the brain fuck interface

/// The default limit for the tape size. This is the value used by methods that don't take it as a parameter
pub const DEFAULT_TAPE_SIZE_LIMIT: usize = 30_000;

#[derive(Debug, Clone, PartialEq, Eq)]
/// A brainfuck interpreter. Read the [module level documentation](self) for more
pub struct Interpreter<'a, I> {
    code: &'a [u8],
    instr_ptr: usize,
    tape: Vec<i8>,
    data_ptr: usize,
    tape_size_limit: usize,
    input: I,
}

impl<'a> Interpreter<'a, InputBuffer> {
    /// Construct an `Interpreter` from an ASCII string of code with an empty input buffer
    /// This methods sets the tape size limit to [its default value](DEFAULT_TAPE_SIZE_LIMIT)
    pub fn from_ascii_with_input_buffer(code: &'a [u8]) -> Self {
        Self::from_ascii_with_input_buffer_and_tape_limit(code, DEFAULT_TAPE_SIZE_LIMIT)
    }

    /// Construct an `Interpreter` from an ASCII string of code with an empty input buffer,
    /// setting the tape size limit to the specified value
    pub fn from_ascii_with_input_buffer_and_tape_limit(
        code: &'a [u8],
        tape_size_limit: usize,
    ) -> Self {
        Self {
            code,
            instr_ptr: 0,
            tape: Vec::new(),
            data_ptr: 0,
            tape_size_limit,
            input: InputBuffer(VecDeque::new()),
        }
    }

    /// Add a byte to the input buffer of this interpreter
    pub fn add_input(&mut self, input: i8) {
        self.input.0.push_back(input);
    }
}

impl<'a, I: BootlegRead> Interpreter<'a, I> {
    /// Construct an interpreter from an ASCII string of code, a source of input bytes, and a tape size limit
    pub fn from_ascii_with_tape_limit(code: &'a [u8], input: I, tape_size_limit: usize) -> Self {
        Self {
            code,
            instr_ptr: 0,
            tape: Vec::new(),
            data_ptr: 0,
            tape_size_limit,
            input,
        }
    }

    /// Constructs an interpreter from an ASCII string of code, a source of input bytes, and [the default tape size limit](DEFAULT_TAPE_SIZE_LIMIT)
    pub fn from_ascii(code: &'a [u8], input: I) -> Self {
        Self::from_ascii_with_tape_limit(code, input, DEFAULT_TAPE_SIZE_LIMIT)
    }

    /// Advance the interpreter by one instruction.
    /// A return value of Ok(None) indicates succesful termination of the interpreter
    pub fn advance(&mut self) -> Result<Option<Status>, ProgramError> {
        let &opcode = match self.code.get(self.instr_ptr) {
            Some(opcode) => opcode,
            None => return Ok(None),
        };

        match opcode {
            b'>' => self.data_ptr += 1,

            b'<' => {
                self.data_ptr = self
                    .data_ptr
                    .checked_sub(1)
                    .ok_or(ProgramError::DataPointerUnderflow)?;
            }

            b'+' => {
                let val = self
                    .get_or_resize_tape_mut()
                    .ok_or(ProgramError::TapeSizeExceededLimit)?;
                *val = val.wrapping_add(1)
            }

            b'-' => {
                let val = self
                    .get_or_resize_tape_mut()
                    .ok_or(ProgramError::TapeSizeExceededLimit)?;
                *val = val.wrapping_sub(1)
            }

            b'.' => {
                self.instr_ptr += 1;
                return Ok(Some(Status::Output(self.get_at_data_ptr())));
            }

            b',' => match self.input.bootleg_read() {
                Ok(Some(num)) => {
                    let cell = self
                        .get_or_resize_tape_mut()
                        .ok_or(ProgramError::TapeSizeExceededLimit)?;
                    *cell = num;
                }
                Ok(None) => return Ok(Some(Status::NeedsInput)),
                Err(_) => return Err(ProgramError::InputReadError),
            },

            b'[' => {
                if self.get_at_data_ptr() == 0 {
                    self.instr_ptr = self
                        .get_matching_closing_bracket(self.instr_ptr)
                        .ok_or(ProgramError::UnmatchedOpeningBracket)?
                    //Instruction pointer will be incremented by 1 after the match
                }
            }

            b']' => {
                if self.get_at_data_ptr() != 0 {
                    self.instr_ptr = self
                        .get_matching_opening_bracket(self.instr_ptr)
                        .ok_or(ProgramError::UnmatchedClosingBracket)?
                    //Instruction pointer will be incremented by 1 after the match
                }
            }

            _ => {} //brainfuck treats all characters it doesn't understand as comments
        }

        self.instr_ptr += 1;

        Ok(Some(Status::Continue))
    }

    /// Advances the interpreter until the next IO operation. See [`advance`](Interpreter::advance)  
    pub fn advance_until_io(&mut self) -> Result<Option<IoStatus>, ProgramError> {
        while let Some(status) = self.advance()? {
            match status {
                Status::NeedsInput => return Ok(Some(IoStatus::NeedsInput)),
                Status::Output(out) => return Ok(Some(IoStatus::Output(out))),
                Status::Continue => continue,
            }
        }
        Ok(None)
    }

    /// Executes the interpreter until it halts, writing all return values to the provided `Write` type.
    /// For more granular control, use [`advance`](Interpreter::advance)
    pub fn interpret_with_output<O: Write>(&mut self, mut output: O) -> Result<(), InterpretError> {
        while let Some(status) = self.advance_until_io()? {
            match status {
                IoStatus::NeedsInput => return Err(InterpretError::EndOfInput),
                IoStatus::Output(out) => match output.write(&[out as u8]) {
                    Ok(0) => return Err(InterpretError::OutputBufferFull),
                    Ok(_) => continue,
                    Err(_) => return Err(InterpretError::OutputWriteError),
                },
            }
        }
        Ok(())
    }

    fn get_or_resize_tape_mut(&mut self) -> Option<&mut i8> {
        if self.data_ptr > self.tape_size_limit {
            return None;
        }
        if self.data_ptr >= self.tape.len() {
            self.tape.resize(self.data_ptr + 1, 0);
        }
        Some(&mut self.tape[self.data_ptr])
    }

    fn get_at_data_ptr(&self) -> i8 {
        //No need to resize the tape to read: if the tape doesn't extend that far already, it holds a value of 0
        self.tape.get(self.data_ptr).copied().unwrap_or(0)
    }

    fn get_matching_closing_bracket(&mut self, opening: usize) -> Option<usize> {
        self.code[opening..]
            .iter()
            .zip(opening..)
            .scan(0, |counter, (char, index)| {
                match char {
                    b'[' => *counter += 1,
                    b']' => *counter -= 1,
                    _ => {}
                };
                Some((*counter, index))
            })
            .find_map(
                |(counter, index)| {
                    if counter == 0 {
                        Some(index)
                    } else {
                        None
                    }
                },
            )
    }

    fn get_matching_opening_bracket(&mut self, closing: usize) -> Option<usize> {
        self.code[..closing + 1]
            .iter()
            .zip(0..closing + 1)
            .rev()
            .scan(0, |counter, (char, index)| {
                match char {
                    b']' => *counter += 1,
                    b'[' => *counter -= 1,
                    _ => {}
                };
                Some((*counter, index))
            })
            .find_map(
                |(counter, index)| {
                    if counter == 0 {
                        Some(index)
                    } else {
                        None
                    }
                },
            )
    }
}

/// A convenience function for interpreting brainfuck code with a given input and output source.
/// For more information, consult [the module level documentation](self)
pub fn interpret_with_io<I: BootlegRead, O: Write>(
    code: &[u8],
    input: I,
    output: O,
) -> Result<(), InterpretError> {
    Interpreter::from_ascii(code, input).interpret_with_output(output)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
///The result of advancing the interpreter by one step, assuming it didn't terminate
pub enum Status {
    NeedsInput,
    Output(i8),
    Continue,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// The result of advancing the interpreter until the next IO operation, assuming it didn't terminate
pub enum IoStatus {
    NeedsInput,
    Output(i8),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// An error that occurred while the interpreter was advancing
pub enum ProgramError {
    DataPointerUnderflow,
    InputReadError,
    UnmatchedOpeningBracket,
    UnmatchedClosingBracket,
    TapeSizeExceededLimit,
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ProgramError::DataPointerUnderflow => "data pointer underflow",
                ProgramError::InputReadError => "input read error",
                ProgramError::UnmatchedOpeningBracket => "unmatched `[`",
                ProgramError::UnmatchedClosingBracket => "unmatched `]`",
                ProgramError::TapeSizeExceededLimit => "tape size exceeded",
            }
        )
    }
}
impl Error for ProgramError {}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// An error that occurred while the interpreter was being run start-to-end all in one go
pub enum InterpretError {
    ProgramError(ProgramError),
    EndOfInput,
    OutputBufferFull,
    OutputWriteError,
}

impl Display for InterpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InterpretError::ProgramError(e) => write!(f, "program error: {}", e),
            InterpretError::EndOfInput => write!(f, "unexpected end of input"),
            InterpretError::OutputBufferFull => write!(f, "output buffer full"),
            InterpretError::OutputWriteError => write!(f, "output write error"),
        }
    }
}
impl Error for InterpretError {}

impl From<ProgramError> for InterpretError {
    fn from(e: ProgramError) -> Self {
        InterpretError::ProgramError(e)
    }
}

/// A bootlegged version of the standard library's read trait, so as to allow the interpreter to be generic over any `Read`
/// type, as well as over an input buffer.
pub trait BootlegRead {
    type Error;
    fn bootleg_read(&mut self) -> Result<Option<i8>, Self::Error>;
}

impl<T: Read> BootlegRead for T {
    type Error = std::io::Error;
    fn bootleg_read(&mut self) -> Result<Option<i8>, Self::Error> {
        let mut buffer = [0];
        match self.read(&mut buffer) {
            Ok(0) => Ok(None),
            Ok(_) => Ok(Some(buffer[0] as i8)),
            Err(e) => Err(e),
        }
    }
}

/// A wrapper around a `VecDeque`, to be able to implement `BootlegRead` for it
struct InputBuffer(VecDeque<i8>);

impl BootlegRead for InputBuffer {
    type Error = std::convert::Infallible;
    fn bootleg_read(&mut self) -> Result<Option<i8>, Self::Error> {
        Ok(self.0.pop_front())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn adder() {
        let mut interpreter = Interpreter {
            code: b"[->+<]", //Source: https://en.wikipedia.org/wiki/Brainfuck
            instr_ptr: 0,
            tape: vec![10, 5],
            data_ptr: 0,
            tape_size_limit: DEFAULT_TAPE_SIZE_LIMIT,
            input: std::io::empty(),
        };

        while let Some(status) = interpreter.advance_until_io().expect("Unexpected error") {
            match status {
                IoStatus::NeedsInput => panic!("Requested input in an IO-less program"),
                IoStatus::Output(_) => panic!("Produced output in an IO-less program"),
            }
        }

        assert_eq!(interpreter.tape, vec![0, 15]);
    }

    #[test]
    fn hello_world() {
        let mut interpreter = Interpreter::from_ascii(
            b"++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.", 
            std::io::empty(),
        );

        let mut string = Vec::new();
        interpreter
            .interpret_with_output(&mut string)
            .expect("Failed to write to output buffer");
        assert_eq!(string, b"Hello World!\n");
    }

    #[test]
    fn with_input_buffer() {
        let mut interpreter = Interpreter::from_ascii_with_input_buffer(b"+++++.>,[-<->].");
        let output = match interpreter
            .advance_until_io()
            .expect("Unexpected error")
            .expect("Unexpected termination")
        {
            IoStatus::NeedsInput => panic!("Unexpected input request"),
            IoStatus::Output(out) => out,
        };

        assert_eq!(
            interpreter.advance_until_io(),
            Ok(Some(IoStatus::NeedsInput))
        );

        interpreter.add_input(output);

        assert_eq!(
            interpreter.advance_until_io(),
            Ok(Some(IoStatus::Output(0)))
        );
        assert_eq!(interpreter.advance_until_io(), Ok(None));
    }

    #[test]
    fn hit_tape_size_limit() {
        let mut interpreter =
            Interpreter::from_ascii_with_tape_limit(b"+>+>+>+>+>", std::io::empty(), 1);
        let result = interpreter.interpret_with_output(std::io::sink());
        assert_eq!(
            result,
            Err(InterpretError::ProgramError(
                ProgramError::TapeSizeExceededLimit
            ))
        );
    }

    #[test]
    fn positive_integer_overflow() {
        interpret_with_io(b"+[+]", std::io::empty(), std::io::sink()).unwrap();
    }

    #[test]
    fn negative_integer_overflow() {
        interpret_with_io(b"-", std::io::empty(), std::io::sink()).unwrap();
    }
}
