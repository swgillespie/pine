extern crate pine_common;
extern crate pine_syntax;
extern crate pine_typecheck;
extern crate pine_trans;
extern crate rustc_serialize;
extern crate env_logger;
extern crate getopts;
extern crate time;

mod options;
mod driver;

use std::env;
use std::process;

fn main() {
    env_logger::init().unwrap();
    let args : Vec<_> = env::args().collect();
    let opts = options::initialize_command_line_opts();
    let compile_options = options::parse_options(&opts, &args[1..]);
    if compile_options.display_help {
        print_usage(&opts);
        process::exit(0);
    }

    if compile_options.input_files.len() == 0 {
        println!("pinec: error: no input files");
        process::exit(1);
    }

    let mut session = driver::Session::new(compile_options);
    driver::do_compilation(&mut session);

    let exit_code = if session.diagnostics.len() == 0 {
        0
    } else {
        1
    };
    for diag in session.diagnostics.iter() {
        println!("{}", diag);
    }

    process::exit(exit_code);
}

fn print_usage(options: &getopts::Options) {
    let brief = "Usage: pinec [options]";
    print!("{}", options.usage(brief));
}
