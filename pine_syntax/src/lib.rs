#![feature(custom_derive)]

//! The `pine_syntax` crate provides four functions that tokenize and parse streams of characters.
//! Each function is parameterized by a charater iterator for ease of use. `tokenize` returns a Lexer
//! struct, which is itself an iterator and the argument to the four `parse` methods, which will return
//! an AST upon success or an error upon failure.

extern crate pine_common;
extern crate rustc_serialize;

mod lexer;
mod parser;
pub mod ast;
pub mod visitor;

pub use lexer::Lexer;
pub use lexer::Token;
pub use lexer::TokenType;

use parser::Parser;
use ast::{CompilationUnit, SpannedFunction, SpannedExpression};

/// Returns an iterator that tokenizes a character stream according to Pine's lexical specification.
/// The actual tokenization is done lazily as the Lexer iterator is consumed.
pub fn tokenize<T: Iterator<Item=char>>(filename: String, iter: T) -> Lexer<T> {
    Lexer::new(filename, iter)
}

/// Parses a compilation unit (a collection of functions) from a lexer and returns a CompilationUnit
/// AST node if successful, and an error if unsuccessful.
pub fn parse_compile_unit<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<CompilationUnit, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    parser.compilation_unit()
}

/// Parses a single function from a lexer and returns a SpannedFunction AST node if successful, and an error if unsuccessful.
pub fn parse_function<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<SpannedFunction, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    let function = parser.function();
    let _ = try!(parser.assert_eof());
    function
}

/// Parses a single expression from a lexer and returns a SpannedExpression AST node if successful, and an error if unsuccessful.
/// It is an error for the lexer to not be on an EOF at the end of the first parsed expression. This means that a series of expressions
/// such as `5 6 7` will not parse, despite being three valid expressions side-by-side.
pub fn parse_expression<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<SpannedExpression, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    let expr = parser.expression();
    let _ = try!(parser.assert_eof());
    expr
}
