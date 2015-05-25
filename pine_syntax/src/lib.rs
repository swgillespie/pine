#![feature(custom_derive)]

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

pub fn tokenize<T: Iterator<Item=char>>(filename: String, iter: T) -> Lexer<T> {
    Lexer::new(filename, iter)
}

pub fn parse_compile_unit<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<CompilationUnit, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    parser.compilation_unit()
}

pub fn parse_function<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<SpannedFunction, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    let function = parser.function();
    let _ = try!(parser.assert_eof()); 
    function
}

pub fn parse_expression<T: Iterator<Item=char>>(lexer: Lexer<T>) -> Result<SpannedExpression, pine_common::CompileDiagnostic> {
    let mut parser = Parser::new(lexer);
    let expr = parser.expression();
    let _ = try!(parser.assert_eof());
    expr
}
