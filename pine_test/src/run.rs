use std::path::Path;
use std::process::Command;
use std::borrow::Borrow;
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use regex::Regex;

use super::Config;

pub fn run_cpass_test(config: &Config, file: &Path) {
    // inovke the compiler, assert that the compiler exits
    // with a zero exit code.
    debug!("running cpass test on {:?}", file);
    let cmd = Command::new(&config.pinec)
        .arg(file)
        .arg("--no-trans")
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
    object_file.push("object.o");
    let mut executable_file = this_folder.clone();
    executable_file.push("object");
    let expected_output = calculate_expected_output(file);
    // remove the output file if it exists.
    // invoke pinec to produce the object file
    let pinec = &config.pinec;
    let cmd = Command::new(pinec)
        .arg(file)
        .arg("-o")
        .arg(&object_file)
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
        .output()
        .unwrap();
    if clang.status.code().unwrap() != 0 {
        panic!("clang exited with non-zero exit code");
    }
    // finally, invoke the generated executable.
    let compiled = Command::new(&executable_file)
        .output()
        .unwrap();
    // clear out the compiled stuff
    fs::remove_file(&object_file).unwrap();
    fs::remove_file(&executable_file).unwrap();
    if let Some(output) = expected_output {
        let stdout = String::from_utf8_lossy(&cmd.stdout).into_owned();
        if stdout != output {
            panic!("expected output of program to be {}, got {}", stdout, output);
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
    if let Some(capture) = regex.captures(&buf) {
        Some(capture.name("output").unwrap().to_string())
    } else {
        None
    }
}
