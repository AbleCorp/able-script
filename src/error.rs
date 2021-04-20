use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Error {
    pub kind: ErrorKind,
    pub position: Range<usize>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    SyntaxError,
}

impl Error {
    pub fn panic(&self, span: &str) {
        println!("{:?} occured at {:?}", self.kind, self.position);
        println!("    {}", &span);
    }
}
