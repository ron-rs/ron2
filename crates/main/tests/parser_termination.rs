//! The parser must terminate (and not grow without bound) on any input.
//!
//! Each case runs on its own thread with a deadline, so a regression to an
//! infinite loop fails the test instead of hanging the suite.

use std::{sync::mpsc, thread, time::Duration};

const OPENERS: [&str; 6] = ["[", "(", "{", "Foo(", "Some(", "(x: "];
const CLOSERS: [&str; 3] = [")", "]", "}"];

/// Parse `source` strictly and lossily, failing if either takes too long.
fn assert_terminates(source: String) {
    let (tx, rx) = mpsc::channel();
    let input = source.clone();
    thread::Builder::new()
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            let strict = ron2::ast::parse_document(&input).is_ok();
            let (_, errors) = ron2::ast::parse_document_lossy(&input);
            let _ = tx.send((strict, errors.len()));
        })
        .unwrap();
    match rx.recv_timeout(Duration::from_secs(10)) {
        Ok((_, errors)) => assert!(errors < 10_000, "{errors} errors for {source:?}"),
        Err(_) => panic!("parser did not terminate for {source:?}"),
    }
}

#[test]
fn mismatched_closers_at_and_below_depth_limit() {
    for depth in [1, 2, 127, 128, 129, 200] {
        for opener in OPENERS {
            for closer in CLOSERS {
                assert_terminates(opener.repeat(depth) + closer);
                assert_terminates(opener.repeat(depth) + closer + ", 1" + closer);
            }
        }
    }
}

#[test]
fn mixed_openers_with_every_closer() {
    let nested: String = OPENERS.iter().cycle().take(300).copied().collect();
    for closer in CLOSERS {
        assert_terminates(nested.clone() + closer);
        assert_terminates(nested.clone() + &closer.repeat(400));
    }
}

#[test]
fn garbage_inside_containers() {
    for opener in OPENERS {
        for junk in [",", ":", ",,", "@", "#", "=", "!", "x:", ": :", "\"", "/*"] {
            for depth in [1, 129] {
                assert_terminates(opener.repeat(depth) + junk);
                assert_terminates(opener.repeat(depth) + junk + ")]}");
            }
        }
    }
}

#[test]
fn lsp_style_partial_documents() {
    for source in [
        "(x: ",
        "(x: [1, ",
        "Foo(x: Bar(",
        "{\"a\": ",
        "{\"a\" ",
        "[Some(",
        "#![type = ",
        "#![enable(",
        "(x: 1, y",
        "(x: 1, @: 2, ]",
    ] {
        assert_terminates(source.to_string());
    }
}
