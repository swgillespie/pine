use pine_common::{CompileDiagnostic, Severity, Span, Position};
use pine_syntax::ast::CompilationUnit;
use pine_syntax;
use pine_typecheck::typed_ast::{TypedCompilationUnit, TypedItem};
use pine_typecheck::types::{self, Type, TypeConst, Types};
use pine_typecheck;
use pine_trans;
use rustc_serialize::json;
use options::CompilationOptions;
use time;

use std::io::prelude::*;
use std::fs::File;
use std::convert::AsRef;
use std::process::Command;
use std::fs;

pub struct Session {
    pub options: CompilationOptions,
    pub filename: String,
    pub diagnostics: Vec<CompileDiagnostic>,
    pub entry_point: Option<usize>
}

impl Session {
    pub fn new(options: CompilationOptions) -> Session {
        Session {
            options: options,
            filename: "".to_string(),
            diagnostics: vec![],
            entry_point: None
        }
    }

    pub fn error<T: AsRef<str>>(&mut self, message: T) {
        let ref_msg = message.as_ref();
        let diagnostic = CompileDiagnostic {
            filename: self.filename.clone(),
            span: Span(Position(1, 0), Position(1, 0)),
            severity: Severity::Error,
            message: ref_msg.to_string()
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
        ast_generation(session)
    });
    let asts = match maybe_asts {
        Some(ast) => ast,
        None => return
    };

    let (pass_2_elapsed_time, maybe_typed_asts) = record_time(|| {
        type_resolution(session, &asts)
    });
    let mut typed_asts = match maybe_typed_asts {
        Some(ty) => ty,
        None => return
    };

    if session.options.print_functions {
        print_types(&typed_asts);
    }

    let (pass_3_elapsed_time, success) = record_time(|| {
        identify_main_function(session, &mut typed_asts)
    });

    if !success || session.options.no_trans {
        return;
    }

    let (pass_4_elapsed_time, mut monomorphized_asts) = record_time(|| {
        monomorphize(session, &typed_asts)
    });

    let (pass_5_elapsed_time, _) = record_time(|| {
        translate(session, &mut monomorphized_asts)
    });

    let (pass_6_elapsed_time, _) = record_time(|| {
        optimization_passes(session)
    });

    if session.options.ast_as_json {
        let json = json::as_pretty_json(&monomorphized_asts);
        println!("{}", json);
    }

    if session.options.print_times {
        println!("ast generation               - \
                  {:>10} ns {:>12} ms",
                 pass_1_elapsed_time,
                 pass_1_elapsed_time as f64 / 1_000_000f64);
        println!("type resolution              - \
                  {:>10} ns {:>12} ms",
                 pass_2_elapsed_time,
                 pass_2_elapsed_time as f64 / 1_000_000f64);
        println!("main function identification - \
                  {:>10} ns {:>12} ms",
                 pass_3_elapsed_time,
                 pass_3_elapsed_time as f64 / 1_000_000f64);
        println!("monomorphization             - \
                  {:>10} ns {:>12} ms",
                 pass_4_elapsed_time,
                 pass_4_elapsed_time as f64 / 1_000_000f64);
        println!("translation to llvm ir       - \
                  {:>10} ns {:>12} ms",
                 pass_5_elapsed_time,
                 pass_5_elapsed_time as f64 / 1_000_000f64);
        println!("llvm optimization            - \
                  {:>10} ns {:>12} ms",
                 pass_6_elapsed_time,
                 pass_6_elapsed_time as f64 / 1_000_000f64);
    }
}

fn ast_generation(session: &mut Session) -> Option<CompilationUnit> {
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

fn type_resolution(session: &mut Session,
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

fn identify_main_function(session: &mut Session,
                          typed_unit: &mut TypedCompilationUnit) -> bool {
    let mut main_function = None;
    for (idx, item) in typed_unit.iter_mut().enumerate() {
        if let &mut TypedItem::Function(ref mut func) = item {
            if func.name == "main" {
                if let Some(_) = main_function {
                    session.error("multiple `main` functions are not allowed");
                    return false;
                } else {
                    main_function = Some(func);
                    session.entry_point = Some(idx);
                }
            }
        } else if let &mut TypedItem::ExternFunction(ref mut extern_fn) = item {
            if extern_fn.name == "main" {
                session.error("`main` function cannot be extern");
                return false;
            }
        }
    }
    if let Some(ref mut f) = main_function {
        // unify main with type [] -> int, as expected for an entry
        // point
        let function_type = Type::Function(f.parameter_types.clone(),
                                           Box::new(f.return_type.clone()));
        let main_type = Type::Function(vec![], Box::new(Type::Const(TypeConst::Int)));
        let subst = match types::unify(&function_type, &main_type) {
            Ok(subst) => subst,
            Err(_) => {
                session.error(format!("unable to unify type `{}` with \
                                       type {}, the type that `main` \
                                       must have", function_type, main_type));
                return false;
            }
        };

        // apply the unifying substitution with [] -> int to main
        f.parameter_types.apply_subst(&subst);
        f.return_type.apply_subst(&subst);
    } else {
        session.error("no `main` function defined");
        return false;
    }
    return true;
}

fn monomorphize(session: &Session, asts: &TypedCompilationUnit) -> TypedCompilationUnit {
    pine_trans::monomorphize(session.entry_point.unwrap(), asts)
}

fn translate(session: &Session, asts: &mut TypedCompilationUnit) {
    let module = pine_trans::translate(asts, &session.filename);
    module.verify();
    let output_file = match session.options.output_file {
        Some(ref path) => path.to_str()
            .unwrap()
            .to_string(),
        None => "a.out".to_string()
    };
    let bitcode = output_file + ".bc";
    module.write_to_file(&bitcode);
}

fn optimization_passes(session: &Session) {
    let output_file = match session.options.output_file {
        Some(ref path) => path.to_str()
            .unwrap()
            .to_string(),
        None => "a.out".to_string()
    };

    let object_file = output_file.clone();
    let bitcode_file = output_file.clone() + ".bc";
    // TODO - a "real" compiler would use the LLVM C api to build up
    // a list of passes and run them on the module to produce a new
    // module. I'd like to do this in the future.
    // For now, in the interest of development speed, we will invoke
    // `opt` directly and have it do optimization and invoke `llc`
    // to translate the llvm bitcode to an object file.
    let opt_command = Command::new("opt")
        .arg("-O2")
        .arg(&bitcode_file)
        .arg("-o")
        .arg(&object_file)
        .output()
        .ok()
        .expect("failed to invoke `opt`!");
    if !opt_command.status.success() {
        panic!("opt exited with non-zero exit code");
    }
    let command = Command::new("llc")
        .arg("-filetype=obj")
        .arg("-o")
        .arg(&object_file)
        .arg(&bitcode_file)
        .output()
        .ok()
        .expect("failed to invoke `llc!`");
    if !command.status.success() {
        // TODO proper error handling
        panic!("llc exited with non-zero exit code");
    }
    // we're done with compilation!
    fs::remove_file(output_file + ".bc").unwrap();
}

fn print_types(asts: &TypedCompilationUnit) {
    for typed_item in asts.iter() {
        if let &TypedItem::Function(ref typed_function) = typed_item {
            let ty = Type::Function(typed_function.parameter_types.clone(),
                                    Box::new(typed_function.return_type.clone()));
            println!("{} :: {}", typed_function.name, ty);
        }
    }
}

fn record_time<T, F: FnMut() -> T>(mut func: F) -> (u64, T) {
    let start = time::precise_time_ns();
    let result = func();
    let stop = time::precise_time_ns();
    (stop - start, result)
}
