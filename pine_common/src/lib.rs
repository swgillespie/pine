extern crate rustc_serialize;

use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone, Debug, RustcEncodable)]
pub struct Position(pub u32, pub u32);

#[derive(PartialEq, Eq, Copy, Clone, Debug, RustcEncodable)]
pub struct Span(pub Position, pub Position);

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Span(Position(start_line, start_col), Position(end_line, end_col)) = *self;
        write!(f, "({}: {}, {}: {})", start_line, start_col, end_line, end_col)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone, Debug, RustcEncodable, RustcDecodable)]
pub enum Severity {
    Warning,
    Error,
    Fatal
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Severity::Warning => write!(f, "warning"),
            Severity::Error => write!(f, "error"),
            Severity::Fatal => write!(f, "fatal")
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug, RustcEncodable)]
pub struct CompileDiagnostic {
    pub filename: String,
    pub span: Span,
    pub severity: Severity,
    pub message: String
}

impl fmt::Display for CompileDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {} {}: {}",
               self.filename,
               self.span,
               self.severity,
               self.message)
    }
}
