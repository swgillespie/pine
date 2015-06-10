use pine_common::{CompileDiagnostic, Severity, Span, Position};
use pine_syntax::ast::CompilationUnit;
use pine_syntax;
use pine_typecheck::typed_ast::TypedCompilationUnit;
use pine_typecheck::types::Type;
use pine_typecheck;
use options::CompilationOptions;
use time;

use std::io::prelude::*;
use std::fs::File;

pub struct Session {
    pub options: CompilationOptions,
    pub filename: String,
    pub diagnostics: Vec<CompileDiagnostic>
}

impl Session {
    pub fn new(options: CompilationOptions) -> Session {
        Session {
            options: options,
            filename: "".to_string(),
            diagnostics: vec![]
        }
    }

    pub fn error(&mut self, message: &str) {
        let diagnostic = CompileDiagnostic {
            filename: self.filename.clone(),
            span: Span(Position(0, 0), Position(0, 0)),
            severity: Severity::Error,
            message: message.to_string()
        };
        self.diagnostics.push(diagnostic);
    }

    pub fn add_diagnostic(&mut self, diagnostic: CompileDiagnostic) {
        self.diagnostics.push(diagnostic);
    }
}

pub fn do_compilation(session: &mut Session) {
    // only one file for now.
    assert_eq!(session.options.input_files.len(), 1);
    let (pass_1_elapsed_time, maybe_asts) = record_time(|| {
        pass_1_ast_generation(session)
    });
    let asts = match maybe_asts {
        Some(ast) => ast,
        None => return
    };

    let (pass_2_elapsed_time, maybe_typed_asts) = record_time(|| {
        pass_2_type_resolution(session, &asts)
    });

    let typed_asts = match maybe_typed_asts {
        Some(ty) => ty,
        None => return
    };

    if session.options.print_functions {
        print_types(&typed_asts);
    }

    if session.options.print_times {
        println!("ast generation  - {:>10} ns {:>12} ms",
                 pass_1_elapsed_time,
                 pass_1_elapsed_time as f64 / 1_000_000f64);
        println!("type resolution - {:>10} ns {:>12} ms",
                 pass_2_elapsed_time,
                 pass_2_elapsed_time as f64 / 1_000_000f64);
    }
}

fn pass_1_ast_generation(session: &mut Session) -> Option<CompilationUnit> {
    // TODO - when adding support for more than one file,
    // remove this.
    session.filename = session.options.input_files[0].clone();
    let mut file = match File::open(&session.filename) {
        Ok(f) => f,
        Err(e) => {
            session.error(&format!("{}", e));
            return None;
        }
    };
    let mut buf = String::new();
    match file.read_to_string(&mut buf) {
        Ok(_) => {},
        Err(e) => {
            session.error(&format!("{}", e));
            return None;
        }
    }
    let lexer = pine_syntax::tokenize(session.filename.clone(), buf.chars());
    let ast = match pine_syntax::parse_compile_unit(lexer) {
        Ok(unit) => unit,
        Err(e) => {
            session.add_diagnostic(e);
            return None;
        }
    };
    Some(ast)
}

fn pass_2_type_resolution(session: &mut Session,
                          units: &CompilationUnit) -> Option<TypedCompilationUnit>
{
    let mut binder = pine_typecheck::TypeBinder::new(session.filename.clone());
    let typed_asts = match pine_typecheck::typecheck_compilation_unit(&mut binder, units) {
        Ok(t) => t,
        Err(e) => {
            session.add_diagnostic(e);
            return None;
        }
    };
    Some(typed_asts)
}

fn print_types(asts: &TypedCompilationUnit) {
    for typed_function in asts.iter() {
        let ty = Type::Function(typed_function.parameter_types.clone(),
                                Box::new(typed_function.return_type.clone()));
        println!("{} :: {}", typed_function.name, ty);
    }
}

fn record_time<T, F: FnMut() -> T>(mut func: F) -> (u64, T) {
    let start = time::precise_time_ns();
    let result = func();
    let stop = time::precise_time_ns();
    (stop - start, result)
}
