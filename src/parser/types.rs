use crate::parser::{
    ast::GenericIdentifier,
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

use super::Parser;
impl Parser {
    ///Parses a type.
    pub fn parse_type(&mut self) -> Result<GenericIdentifier, ParseError> {
        let Token {
            kind: TokenKind::Identifier(ident),
            mut span,
        } = self.expect(&TokenKind::Identifier("".to_string()))?
        else {
            unreachable!()
        };
        if let Token {
            kind: TokenKind::Lt,
            ..
        } = self.peek()?
        {
            let mut generics = Vec::new();
            self.eat()?;
            loop {
                if let TokenKind::Gt = self.peek()?.kind {
                    let end = self.eat()?.span;
                    span.end = end.end;
                    break;
                }
                let ty = self.parse_type()?;
                generics.push(ty);
            }
            Ok(GenericIdentifier {
                generic: Some(generics),
                identifier: ident,
                span: span,
            })
        } else {
            Ok(GenericIdentifier {
                identifier: ident,
                generic: None,
                span,
            })
        }
    }
}
