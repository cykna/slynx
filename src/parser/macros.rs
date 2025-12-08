use crate::parser::{
    ast::{ASTDeclaration, ASTDeclarationKind, MacroCallDecl, Span},
    error::ParseError,
    lexer::tokens::TokenKind,
};

pub use super::Parser;
impl Parser {
    ///Parses a declaration level macro with the provided `name` and `span`
    pub fn parse_macro(&mut self, name: String, span: Span) -> Result<ASTDeclaration, ParseError> {
        let initial = self.eat()?;

        if let TokenKind::RBrace | TokenKind::RParen = initial.kind {
            let req_paren = matches!(initial.kind, TokenKind::RParen);
            let decls = self.parse_declarations()?;
            let tk = if req_paren {
                self.expect(&TokenKind::LParen)?
            } else {
                self.expect(&TokenKind::RParen)?
            };

            Ok(ASTDeclaration {
                kind: ASTDeclarationKind::MacroCall(MacroCallDecl { name, args: decls }),
                span: Span {
                    start: span.start,
                    end: tk.span.end,
                },
            })
        } else {
            Err(ParseError::UnexpectedToken(
                initial,
                "Expecintg '(' or '{'".to_string(),
            ))
        }
    }
}
