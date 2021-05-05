// NOTE(Able): This is the brain fuck interface

use brainfuck::program::Program;
use brainfuck::tape::ArrayTape;
use brainfuck::Interpreter;

pub fn bff_eval(function: String, args: String) {
    let mut stdout = "";
    let program = Program::parse("++>+.").unwrap();
    /* This failes currently I guess
            let mut interp = Interpreter::<ArrayTape>::new(program, &mut stdin, &mut stdout);
    */
}
