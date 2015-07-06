use std::path::Path;
use std::process::Command;
use std::borrow::Borrow;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::fmt::Debug;
use regex::Regex;

use super::Config;

pub trait ChainableDebugPrint : Debug {
    // This is a small utility function used to dump a Command
    // object to the debug info channel while still being
    // used in a method chain.
    fn debug_print(&mut self) -> &mut Self {
        debug!("{:?}", self);
        self
    }
}

pub struct FileGuard<'a> {
    filename: &'a Path
}

impl<'a> FileGuard<'a> {
    pub fn new(name: &'a Path) -> FileGuard {
        FileGuard {
            filename: name
        }
    }
}

impl<'a> Drop for FileGuard<'a> {
    fn drop(&mut self) {
        fs::remove_file(self.filename).unwrap();
    }
}

impl ChainableDebugPrint for Command {}

pub fn run_cpass_test(config: &Config, file: &Path) {
    // inovke the compiler, assert that the compiler exits
    // with a zero exit code.
    debug!("running cpass test on {:?}", file);
    let cmd = Command::new(&config.pinec)
        .arg(file)
        .arg("--no-trans")
        .debug_print()
        .output()
        .unwrap();
    if cmd.status.code().unwrap() != 0 {
        let stdout = String::from_utf8_lossy(&cmd.stdout);
        let stderr = String::from_utf8_lossy(&cmd.stderr);
        panic!("pinec exited with error code {}.\nstdout: {}\nstderr: {}",
            cmd.status.code().unwrap(), stdout, stderr);
    }
}

pub fn run_cfail_test(config: &Config, file: &Path) {
    // on the line in which the expected error occurs,
    // the file will have a comment of the form
    // "# error:(error_text_substring)"
    // We extract the error text substring and record the line
    // upon which the comment occured. After we invoke pinec,
    // we parse stdout for a list of diagnostics. For every diagnostic
    // we check and see 1) if the line number is the same as the expected
    // diagnostic and 2) if the diagnostic on that line number matches
    // the substring given in the comment. If either one of those
    // are not true, we fail the test.
    let expected_diagnostics = calculate_expected_diags(file);
    // invoke pinec
    debug!("running cfail test on {:?}", file);
    let cmd = Command::new(&config.pinec)
        .arg(file)
        .arg("--no-trans")
        .debug_print()
        .output()
        .unwrap();
    if cmd.status.code().unwrap() == 0 {
        panic!("pinec exited with error code 0 when there should have been errors");
    }
    let stdout = String::from_utf8_lossy(&cmd.stdout);
    let diagnostics = parse_stdout(stdout.borrow());
    if diagnostics.len() != expected_diagnostics.len() {
        panic!("unexpected number of diagnostics. expected {} diagnostics but got {}. \
                Actual diagnostics:\n{:?}", expected_diagnostics.len(), diagnostics.len(), diagnostics);
    }
    for (line, message) in diagnostics.into_iter() {
        let mut found = false;
        for &(expected_line, ref expected_message) in expected_diagnostics.iter() {
            if expected_line == line && message.contains(expected_message) {
                found = true;
            }
        }
        if !found {
            panic!("unexpected diagnostic on line {}: {}", line, message);
        }
    }
}

fn calculate_expected_diags(file: &Path) -> Vec<(i32, String)> {
    let regex = Regex::new(r".*#\s*error:\s*(?P<message>.*)").unwrap();
    let open_file = File::open(file).unwrap();
    let reader = BufReader::new(open_file);
    let mut diagnostics = vec![];
    for (line_number, line) in reader.lines().enumerate() {
        let actual_line = line.unwrap();
        if let Some(capture) = regex.captures(&actual_line) {
            let messages = capture.name("message").unwrap();
            diagnostics.push(((line_number + 1) as i32, messages.to_string()));
        }
    }
    debug!("expected diagnostics: {:?}", diagnostics);
    diagnostics
}

fn parse_stdout(stdout: &str) -> Vec<(i32, String)> {
    let regex = Regex::new(r".*: \(line (?P<line>\d+), col \d+, line \d+, col \d+\) error: (?P<message>.*)")
        .unwrap();
    let mut diagnostics = vec![];
    for line in stdout.lines() {
        if let Some(capture) = regex.captures(line) {
            let line_number : i32 = capture.name("line")
                .unwrap()
                .parse()
                .unwrap();
            let message = capture.name("message").unwrap();
            diagnostics.push((line_number, message.to_string()));
        }
    }
    diagnostics
}

pub fn run_rpass_test(config: &Config, file: &Path) {
    debug!("running rpass test on {:?}", file);
    let this_folder = file.to_path_buf();
    let mut object_file = this_folder.clone();
    object_file.pop();
    object_file.push("object.o");
    let mut executable_file = this_folder.clone();
    executable_file.pop();
    executable_file.push("object");
    let expected_output = calculate_expected_output(file);
    // remove the output file if it exists.
    // invoke pinec to produce the object file
    let pinec = &config.pinec;
    let cmd = Command::new(pinec)
        .arg(file)
        .arg("-o")
        .arg(&object_file)
        .debug_print()
        .output()
        .unwrap();
    // fail if pinec failed.
    if cmd.status.code().unwrap() != 0 {
        panic!("pinec exited with non-zero exit code. stdout:\n{}",
            String::from_utf8(cmd.stdout).unwrap());
    }

    // invoke the system linker to produce an executable
    // TODO something more general than just clang
    let clang = Command::new("clang")
        .arg(&object_file)
        .arg("-o")
        .arg(&executable_file)
        .arg("-lpinert")
        .arg("-L")
        .arg(&config.pine_runtime)
        .debug_print()
        .output()
        .unwrap();
    if clang.status.code().unwrap() != 0 {
        panic!("clang exited with non-zero exit code");
    }
    let _object_file_guard = FileGuard::new(&object_file);

    // finally, invoke the generated executable.
    let compiled = Command::new(&executable_file)
        .debug_print()
        .output()
        .unwrap();
    let _executable_file_guard = FileGuard::new(&executable_file);
    // clear out the compiled stuff
    if let Some(output) = expected_output {
        let stdout = String::from_utf8(compiled.stdout).unwrap();
        if stdout != output {
            panic!("expected output of program to be {}, got {}", output, stdout);
        }
    } else {
        if compiled.status.code().unwrap() != 0 {
            panic!("no expected output specified and program exited with non-zero exit code");
        }
    }
}

fn calculate_expected_output(file: &Path) -> Option<String> {
    let regex = Regex::new(r".*#\s*output:(?P<output>.*)").unwrap();
    let open_file = File::open(file).unwrap();
    let mut reader = BufReader::new(open_file);
    let mut buf = String::new();
    reader.read_line(&mut buf).unwrap();
    let output = if let Some(capture) = regex.captures(&buf) {
        Some(capture.name("output").unwrap().to_string())
    } else {
        None
    };
    debug!("expected output: {:?}", output);
    output
}
