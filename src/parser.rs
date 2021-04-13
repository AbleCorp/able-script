use crate::tokens::Abool;

pub fn abool2num(abool: Abool) -> i32 {
    match abool {
        Abool::Never => -1,
        Abool::Sometimes => 0,
        Abool::Always => 1,
    }
}
pub fn num2abool(number: i32) -> Abool {
    match number {
        -1 => Abool::Never,
        0 => Abool::Sometimes,
        1 => Abool::Always,
        _ => Abool::Sometimes,
    }
}
