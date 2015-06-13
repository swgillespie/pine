use getopts::Options;
use std::default::Default;

#[derive(Debug)]
pub struct CompilationOptions {
    pub output_file: Option<String>,
    pub print_functions: bool,
    pub display_help: bool,
    pub input_files: Vec<String>,
    pub errors_as_json: bool,
    pub ast_as_json: bool,
    pub print_times: bool
}

impl Default for CompilationOptions {
    fn default() -> CompilationOptions {
        CompilationOptions {
            output_file: None,
            print_functions: false,
            display_help: false,
            input_files: vec![],
            errors_as_json: false,
            ast_as_json: false,
            print_times: false
        }
    }
}

pub fn initialize_command_line_opts() -> Options {
    let mut opts = Options::new();
    opts.optopt("o", "output", "set the output file name", "NAME");
    opts.optflag("t", "types", "output the types of every function to standard out");
    opts.optflag("h", "help", "display this help message");
    opts.optflag("j", "error-json", "output errors as json");
    opts.optflag("a", "ast-json", "output ast as json");
    opts.optflag("b", "benchmark", "output times for every pass");
    return opts;
}

pub fn parse_options(opts: &Options, args: &[String]) -> CompilationOptions {
    let matches = opts.parse(args).unwrap();
    let mut options = CompilationOptions::default();
    options.display_help = matches.opt_present("h");
    options.print_functions = matches.opt_present("t");
    options.errors_as_json = matches.opt_present("j");
    options.ast_as_json = matches.opt_present("a");
    options.output_file = matches.opt_str("o");
    options.input_files = matches.free.clone();
    options.print_times = matches.opt_present("b");
    options
}