#![feature(test)]

extern crate test;
extern crate regex;
#[macro_use]
extern crate log;
extern crate env_logger;

mod run;

use std::env;
use std::process;
use std::convert::AsRef;
use std::path::{PathBuf, Path};
use std::sync::Arc;
use std::fs;

#[derive(Clone)]
pub struct Config {
    pinec: PathBuf,
    sources: PathBuf,
}

fn main() {
    env_logger::init().unwrap();
    let pinec_path = match env::var("PINEC") {
        Ok(value) => PathBuf::from(value),
        Err(_) => abort_with_message("failed to read PINEC environment variable, please set it \
                                      to the path of the PINEC compiler to test.")
    };
    let test_dir = match env::var("PINEC_TEST_SOURCES") {
        Ok(value) => PathBuf::from(value),
        Err(_) => abort_with_message("failed to read PINEC_TEST_SOURCES environment variable, \
                                      please set it to the path of the pine sources to test")
    };
    let config = Config {
        pinec: pinec_path,
        sources: test_dir,
    };

    let shared_conf = Arc::new(config);
    let tests = construct_tests(shared_conf.clone());
    let options = construct_opts(&shared_conf);
    match test::run_tests_console(&options, tests) {
        Ok(true) => {},
        Ok(false) => abort_with_message("there were test failures"),
        Err(e) => abort_with_message(format!("test running failed with error: {:?}", e))
    }
    process::exit(0);
}

fn construct_opts(_: &Config) -> test::TestOpts {
    test::TestOpts {
        filter: None,
        run_ignored: false,
        run_tests: true,
        logfile: None,
        bench_benchmarks: false,
        nocapture: false,
        color: test::AutoColor
    }
}

fn construct_tests(config: Arc<Config>) -> Vec<test::TestDescAndFn> {
    // the test_dir should have the following structure:
    // TEST_DIR
    // |
    // -> compile_fail
    // |
    // -> compile_pass
    // |
    // -> run_pass
    //
    // compile_fail tests assert that the compiler raises
    // appropriate errors. compile_pass tests assert that
    // the compiler allows constructs that should be allowed.
    // run_pass asserts that the compiler compiles a program
    // correctly and runs the output program, checking
    // the standard out of the program to see if the program
    // did what was intended.
    let cfail_tests = construct_cfail_tests(config.clone());
    let cpass_tests = construct_cpass_tests(config.clone());
    let rpass_tests = construct_rpass_tests(config.clone());
    cfail_tests.into_iter()
        .chain(cpass_tests.into_iter())
        .chain(rpass_tests.into_iter())
        .collect()
}

fn test_name(prefix: &str, path: &Path) -> test::TestName {
    test::DynTestName(format!("{}_{}", prefix, path.file_stem().unwrap().to_str().unwrap()))
}

fn construct_cfail_tests(config: Arc<Config>) -> Vec<test::TestDescAndFn> {
    let mut test_dir = config.sources.clone();
    test_dir.push("compile_fail");
    let mut tests = vec![];
    for dir in fs::read_dir(test_dir).unwrap() {
        let entry = dir.unwrap();
        let path = entry.path().clone();
        let child_config = config.clone();
        let test = test::TestDescAndFn {
            desc: test::TestDesc {
                name: test_name("cfail", &path),
                ignore: false,
                should_panic: test::ShouldPanic::No
            },
            testfn: test::DynTestFn(Box::new(move || {
                run::run_cfail_test(&child_config, &path)
            }))
        };
        tests.push(test);
    }
    tests
}

fn construct_cpass_tests(config: Arc<Config>) -> Vec<test::TestDescAndFn> {
    let mut test_dir = config.sources.clone();
    test_dir.push("compile_pass");
    let mut tests = vec![];
    for dir in fs::read_dir(test_dir).unwrap() {
        let entry = dir.unwrap();
        let path = entry.path().clone();
        let child_config = config.clone();
        let test = test::TestDescAndFn {
            desc: test::TestDesc {
                name: test_name("cpass", &path),
                ignore: false,
                should_panic: test::ShouldPanic::No
            },
            testfn: test::DynTestFn(Box::new(move || {
                run::run_cpass_test(&child_config, &path)
            }))
        };
        tests.push(test);
    }
    tests
}

fn construct_rpass_tests(config: Arc<Config>) -> Vec<test::TestDescAndFn> {
    let mut test_dir = config.sources.clone();
    test_dir.push("run_pass");
    let mut tests = vec![];
    for dir in fs::read_dir(test_dir).unwrap() {
        let entry = dir.unwrap();
        let path = entry.path().clone();
        let child_config = config.clone();
        let test = test::TestDescAndFn {
            desc: test::TestDesc {
                name: test_name("rpass", &path),
                ignore: false,
                should_panic: test::ShouldPanic::No
            },
            testfn: test::DynTestFn(Box::new(move || {
                run::run_rpass_test(&child_config, &path)
            }))
        };
        tests.push(test);
    }
    tests
}

fn abort_with_message<T: AsRef<str>>(message: T) -> ! {
    let message_ref = message.as_ref();
    println!("{}", message_ref);
    process::exit(1);
}
