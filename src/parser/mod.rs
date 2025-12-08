pub mod ast;
mod component;
pub mod error;
mod expr;
mod functions;
pub mod lexer;
mod macros;
mod statment;
mod types;

use crate::parser::{
    ast::{ASTDeclaration, ASTDeclarationKind, MacroCallDecl, Span},
    error::ParseError,
    lexer::{
        TokenStream,
        tokens::{Token, TokenKind},
    },
};

pub struct Parser {
    stream: TokenStream,
}

impl Parser {
    ///Creates a new parser instance from the given `stream`
    pub fn new(stream: TokenStream) -> Self {
        Parser { stream }
    }

    pub fn eat(&mut self) -> Result<Token, ParseError> {
        self.stream.next().ok_or(ParseError::UnexpectedEndOfInput)
    }

    pub fn peek_at(&self, idx: usize) -> Result<&Token, ParseError> {
        self.stream
            .stream
            .get(idx)
            .ok_or(ParseError::UnexpectedEndOfInput)
    }

    pub fn peek(&self) -> Result<&Token, ParseError> {
        self.peek_at(0)
    }

    pub fn expect(&mut self, kind: &TokenKind) -> Result<Token, ParseError> {
        let token = self.eat()?;
        if std::mem::discriminant(&token.kind) == std::mem::discriminant(kind) {
            Ok(token)
        } else {
            let kind = match kind {
                TokenKind::Identifier(_) => "a name".to_string(),
                TokenKind::Int(_) => "an integer literal".to_string(),
                TokenKind::Float(_) => "a float literal".to_string(),
                TokenKind::String(_) => "a string literal".to_string(),
                _ => format!("'{}'", kind.to_string()),
            };
            Err(ParseError::UnexpectedToken(token, kind))
        }
    }

    pub fn parse_declarations(&mut self) -> Result<Vec<ASTDeclaration>, ParseError> {
        let mut out = Vec::new();
        while let Ok(token) = self.peek() {
            match &token.kind {
                TokenKind::MacroName(_) => {
                    let Token {
                        kind: TokenKind::MacroName(name),
                        span,
                    } = self.eat()?
                    else {
                        unreachable!();
                    };
                    out.push(self.parse_macro(name, span)?)
                }
                TokenKind::Component => {
                    let Token { span, .. } = self.eat()?;
                    out.push(self.parse_component(span)?)
                }
                TokenKind::Func => {
                    let Token {
                        kind: TokenKind::Func,
                        span,
                    } = self.eat()?
                    else {
                        unreachable!();
                    };
                    out.push(self.parse_func(span)?)
                }
                _ => {
                    return Err(ParseError::UnexpectedToken(
                        self.eat()?,
                        "Either a macro name(a name terminated by '!' such as 'js!'), 'Component' or 'Func'".to_string(),
                    ));
                }
            }
        }
        Ok(out)
    }
}
