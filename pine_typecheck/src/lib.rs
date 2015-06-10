extern crate pine_common;
extern crate pine_syntax;

#[macro_use]
extern crate log;

use pine_syntax::ast::{CompilationUnit,
                       SpannedFunction,
                       SpannedExpression};
use pine_common::CompileDiagnostic;
use typed_ast::{TypedCompilationUnit, TypedFunction, TypedExpression};

pub mod types;
pub mod typed_ast;
mod type_binder;

pub use type_binder::TypeBinder;
//pub mod symbol_table;

pub fn typecheck_compilation_unit(binder: &mut TypeBinder, unit: &CompilationUnit) -> Result<TypedCompilationUnit, CompileDiagnostic> {
    let mut fns = vec![];
    for func in unit.iter() {
        let (_, ty) = try!(binder.visit_function(&func.data));
        fns.push(ty);
    }
    Ok(fns)
}

pub fn typecheck_function(binder: &mut TypeBinder, func: &SpannedFunction) -> Result<TypedFunction, CompileDiagnostic> {
    let (_, ty) = try!(binder.visit_function(&func.data));
    Ok(ty)
}

pub fn typecheck_expression(binder: &mut TypeBinder, expr: &SpannedExpression) -> Result<TypedExpression, CompileDiagnostic> {
    let (_, ty) = try!(binder.visit_expression(expr));
    Ok(ty)
}
