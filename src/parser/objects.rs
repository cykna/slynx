use crate::parser::{Parser, ast::{ASTDeclaration, ASTDeclarationKind, ObjectField, Span}, error::ParseError, lexer::tokens::{Token, TokenKind}};

impl Parser {
    pub fn parse_object(&mut self, start:Span) ->Result<ASTDeclaration, ParseError> {
        let name = self.parse_type()?;
        self.expect(&TokenKind::LBrace)?;
        let mut fields= Vec::new();
        while self.peek()?.kind != TokenKind::RBrace {
            let name = self.parse_typedname()?;
            fields.push(ObjectField {
                visibility: super::ast::PropertyModifier::Public,
                name
            });
            if self.peek()?.kind == TokenKind::RBrace {
                break;
            }else {
                self.expect(&TokenKind::Colon)?;
            }
        }
        let Token {span, .. } = self.expect(&TokenKind::RBrace)?;
        Ok(ASTDeclaration { kind: ASTDeclarationKind::ObjectDeclaration {
            name,
            fields
        }, span: Span { start: start.start, end: span.end } })
    }
}

