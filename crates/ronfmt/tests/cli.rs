//! Integration tests for the ronfmt CLI.

use std::process::{Command, Stdio};
use std::io::Write;

fn ronfmt() -> Command {
    Command::new(env!("CARGO_BIN_EXE_ronfmt"))
}

fn run_with_stdin(input: &str) -> (String, String, i32) {
    let mut child = ronfmt()
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn ronfmt");

    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(input.as_bytes())
        .expect("Failed to write to stdin");

    let output = child.wait_with_output().expect("Failed to wait on child");
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let code = output.status.code().unwrap_or(-1);
    (stdout, stderr, code)
}

fn run_with_stdin_args(input: &str, args: &[&str]) -> (String, String, i32) {
    let mut child = ronfmt()
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to spawn ronfmt");

    child
        .stdin
        .as_mut()
        .unwrap()
        .write_all(input.as_bytes())
        .expect("Failed to write to stdin");

    let output = child.wait_with_output().expect("Failed to wait on child");
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let code = output.status.code().unwrap_or(-1);
    (stdout, stderr, code)
}

// ============================================================================
// Basic formatting
// ============================================================================

#[test]
fn test_formats_stdin() {
    let (stdout, stderr, code) = run_with_stdin("Config(x:1,y:2)");
    assert_eq!(code, 0, "stderr: {stderr}");
    assert_eq!(stdout, "Config(x: 1, y: 2)\n");
}

#[test]
fn test_formats_compact_array() {
    let (stdout, _, code) = run_with_stdin("[1,2,3]");
    assert_eq!(code, 0);
    assert_eq!(stdout, "[1, 2, 3]\n");
}

#[test]
fn test_preserves_comments() {
    let input = "// comment\n42";
    let (stdout, _, code) = run_with_stdin(input);
    assert_eq!(code, 0);
    assert!(stdout.contains("// comment"));
    assert!(stdout.contains("42"));
}

// ============================================================================
// Check mode
// ============================================================================

#[test]
fn test_check_mode_formatted() {
    // Already formatted input
    let (stdout, stderr, code) = run_with_stdin_args("Config(x: 1, y: 2)\n", &["--check"]);
    assert_eq!(code, 0, "Expected success for formatted input. stderr: {stderr}, stdout: {stdout}");
}

#[test]
fn test_check_mode_unformatted() {
    // Unformatted input
    let (_, stderr, code) = run_with_stdin_args("Config(x:1,y:2)", &["--check"]);
    assert_eq!(code, 1, "Expected exit 1 for unformatted input");
    assert!(stderr.contains("not formatted correctly"));
}

// ============================================================================
// Configuration options
// ============================================================================

#[test]
fn test_custom_indent() {
    let (stdout, _, code) = run_with_stdin_args("[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]", &["--width", "10", "--indent", "2"]);
    assert_eq!(code, 0);
    assert!(stdout.contains("  1,"), "Expected 2-space indent in: {stdout}");
}

#[test]
fn test_custom_width() {
    // Short width should force multiline
    let (stdout, _, code) = run_with_stdin_args("[1, 2, 3]", &["--width", "5"]);
    assert_eq!(code, 0);
    assert!(stdout.contains('\n'), "Expected multiline output: {stdout}");
}

#[test]
fn test_large_width() {
    // Large width should keep compact
    let (stdout, _, code) = run_with_stdin_args("[1, 2, 3, 4, 5]", &["--width", "100"]);
    assert_eq!(code, 0);
    assert_eq!(stdout, "[1, 2, 3, 4, 5]\n");
}

// ============================================================================
// Error handling
// ============================================================================

#[test]
fn test_invalid_ron() {
    let (_, stderr, code) = run_with_stdin("[1, 2, ");
    assert_eq!(code, 2, "Expected exit 2 for parse error");
    assert!(stderr.contains("error"), "Expected error message in stderr: {stderr}");
}

// ============================================================================
// Help and version
// ============================================================================

#[test]
fn test_help() {
    let output = ronfmt()
        .arg("--help")
        .output()
        .expect("Failed to run ronfmt");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("Usage:"));
    assert!(stdout.contains("--check"));
    assert!(stdout.contains("--width"));
    assert!(stdout.contains("--indent"));
}

#[test]
fn test_version() {
    let output = ronfmt()
        .arg("--version")
        .output()
        .expect("Failed to run ronfmt");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("ronfmt"));
}
