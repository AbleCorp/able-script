use crate::tokens::{ABOOL, TOKENS};

pub fn abool2num(abool: ABOOL) -> i32 {
    match abool {
        ABOOL::NEVER => -1,
        ABOOL::SOMETIMES => 0,
        ABOOL::ALWAYS => 1,
    }
}
pub fn num2abool(number: i32) -> ABOOL {
    match number {
        -1 => ABOOL::NEVER,
        0 => ABOOL::SOMETIMES,
        1 => ABOOL::ALWAYS,
        _ => ABOOL::SOMETIMES,
    }
}
