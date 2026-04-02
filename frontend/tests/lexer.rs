use frontend::lexer::{Lexer, error::LexerError, tokens::TokenKind};

#[test]
fn tokenizes_integer_with_numeric_separators() {
    let tokens = Lexer::tokenize("1_000_000").expect("integer with separators should tokenize");
    let first = tokens.stream.front().expect("expected one token");

    assert!(matches!(first.kind, TokenKind::Int(1_000_000)));
}

#[test]
fn tokenizes_float_with_numeric_separators() {
    let tokens =
        Lexer::tokenize("2_000_0_0_0.44_00_2").expect("float with separators should tokenize");
    let first = tokens.stream.front().expect("expected one token");

    match &first.kind {
        TokenKind::Float(value) => {
            assert!((value - 20_000_000.440_02).abs() < 0.001);
        }
        other => panic!("expected float token, got {other:?}"),
    }
}

#[test]
fn rejects_numeric_separator_next_to_decimal_point() {
    let err = Lexer::tokenize("1_.0").expect_err("separator next to decimal point should fail");

    assert!(matches!(err, LexerError::MalformedNumber { .. }));
}

#[test]
fn rejects_double_decimal_point() {
    let err = Lexer::tokenize("4..0").expect_err("double decimal point should fail");

    assert!(matches!(err, LexerError::MalformedNumber { .. }));
}
