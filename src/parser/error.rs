use crate::parser::lexer::tokens::Token;

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Token, String),
    UnexpectedEndOfInput,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ParseError::UnexpectedToken(token, expected_ty) => {
                write!(
                    f,
                    "Unexpected token: {token}. Instead, was expecing {expected_ty}",
                )
            }
            ParseError::UnexpectedEndOfInput => write!(f, "Unexpected end of input"),
        }
    }
}

impl std::error::Error for ParseError {}
