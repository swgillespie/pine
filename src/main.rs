extern crate pine_syntax;
extern crate rustc_serialize;
use rustc_serialize::json;

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
        let parse_result = pine_syntax::parse_expression(lexer);
        match parse_result {
            Ok(ast) => println!("{}", json::as_pretty_json(&ast)),
            Err(err) => println!("{}", json::as_pretty_json(&err))
        };
    }
}

