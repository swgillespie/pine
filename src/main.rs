extern crate pine_syntax;
extern crate pine_typecheck;
extern crate rustc_serialize;
use rustc_serialize::json;

use pine_typecheck::type_binder::TypeBinder;
use pine_typecheck::types::{Type, Types};

use std::io::{stdin, stdout, BufRead, Write};

fn main() {
    let input = stdin();
    let mut output = stdout();
    let mut locked_stdin = input.lock();
    loop {
        write!(&mut output, ">>> ").unwrap();
        output.flush().unwrap();
        let mut buf = String::new();
        let _ = locked_stdin.read_line(&mut buf).unwrap();
        if &buf == "exit\n" {
            println!("bye!");
            break;
        }
        let lexer = pine_syntax::tokenize("stdin".to_string(), buf.chars());
        let parse_result = pine_syntax::parse_function(lexer);
        let ast = match parse_result {
            Ok(ast) => ast,
            Err(err) => {
                println!("{}", json::as_pretty_json(&err));
                continue;
            }
        };

        let mut binder = TypeBinder::new("<stdin>".to_string());
        match binder.visit_function(&ast.data) {
            Ok((subst, typed_ast)) => {
                let mut local_ast = typed_ast.clone();
                local_ast.parameter_types.apply_subst(&subst);
                local_ast.return_type.apply_subst(&subst);
                let ty = Type::Function(local_ast.parameter_types.clone(),
                                        Box::new(local_ast.return_type.clone()));
                println!("{}", ty);
            },
            Err(err) => {
                println!("{}", json::as_pretty_json(&err));
            }
        }
    }
}

