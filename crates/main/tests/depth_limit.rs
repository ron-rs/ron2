//! Input nested past the depth limit fails with `ExceededRecursionLimit`.
//!
//! Regression: ron2 0.3.0 used gigabytes of memory and never returned on
//! structs inside sequences nested past the limit. Each case runs on a 2 MiB
//! thread with a deadline, so a regression fails instead of hanging.

use std::{sync::mpsc, thread, time::Duration};

use ron2::{
    ErrorKind, FromRon,
    ast::{MAX_CONVERSION_DEPTH, parse_document, parse_document_lossy},
};

const PREFIX: &str = "A(x: 1, children: [";

/// `count` structs `A`, each inside the previous one's `children`: the
/// innermost `[]` and `1` are `2 * count` levels deep.
fn nested(count: usize) -> String {
    let mut s = PREFIX.repeat(count - 1);
    s.push_str("A(x: 1, children: [])");
    s.push_str(&"])".repeat(count - 1));
    s
}

/// The most structs that fit within the limit.
const FITTING: usize = MAX_CONVERSION_DEPTH / 2;

#[derive(Debug, FromRon)]
struct A {
    x: u32,
    children: Vec<A>,
}

impl A {
    fn depth(&self) -> usize {
        assert_eq!(self.x, 1);
        1 + self.children.iter().map(A::depth).max().unwrap_or(0)
    }
}

/// Run `f` on a 2 MiB thread, failing if it takes longer than 10 seconds.
fn with_deadline<T: Send + 'static>(f: impl FnOnce() -> T + Send + 'static) -> T {
    let (tx, rx) = mpsc::channel();
    thread::Builder::new()
        .stack_size(2 << 20)
        .spawn(move || {
            let _ = tx.send(f());
        })
        .unwrap();
    rx.recv_timeout(Duration::from_secs(10))
        .expect("did not finish in time")
}

/// Byte offset of the first struct past the limit, where every error points.
fn first_too_deep() -> usize {
    FITTING * PREFIX.len()
}

#[track_caller]
fn assert_too_deep(err: &ron2::Error) {
    assert_eq!(err.kind(), &ErrorKind::ExceededRecursionLimit);
    assert_eq!(err.kind().to_string(), "Nesting deeper than 128 levels");
    let span = err.span();
    assert_eq!(
        (span.start_offset, span.end_offset),
        (first_too_deep(), first_too_deep() + 1),
        "the span covers the name of the first struct past the limit"
    );
    assert_eq!((span.start.line, span.start.col), (1, first_too_deep() + 1));
}

#[test]
fn just_under_the_limit_parses() {
    with_deadline(|| {
        let source = nested(FITTING);
        let doc = parse_document(&source).unwrap();
        assert!(doc.value.is_some());
        let (_, errors) = parse_document_lossy(&source);
        assert!(errors.is_empty(), "{errors:?}");
        assert_eq!(A::from_ron(&source).unwrap().depth(), FITTING);
    });
}

#[test]
fn parse_document_past_the_limit() {
    with_deadline(|| {
        for count in [FITTING + 1, 1000, 100_000] {
            let err = parse_document(&nested(count)).unwrap_err();
            assert_too_deep(&err);
        }
    });
}

#[test]
fn parse_document_lossy_past_the_limit() {
    with_deadline(|| {
        for count in [FITTING + 1, 1000, 100_000] {
            let source = nested(count);
            let (doc, errors) = parse_document_lossy(&source);
            assert!(doc.value.is_some());
            assert_eq!(errors.len(), 1, "{count}: {errors:?}");
            assert_too_deep(&errors[0]);
        }
    });
}

#[test]
fn derived_from_ron_past_the_limit() {
    with_deadline(|| {
        for count in [FITTING + 1, 1000, 100_000] {
            let err = A::from_ron(&nested(count)).unwrap_err();
            assert_too_deep(&err);
        }
    });
}
