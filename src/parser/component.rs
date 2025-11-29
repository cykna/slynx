use crate::parser::{
    ast::{
        ASTDeclaration, ASTDeclarationKind, ElementDeffinition, ElementDeffinitionKind,
        PropertyModifier, Span,
    },
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

use super::Parser;
impl Parser {
    fn parse_element_deffinition(&mut self) -> Result<ElementDeffinition, ParseError> {
        let mut span = self.peek()?.span;
        let modifier = match self.peek()?.kind {
            TokenKind::Pub => {
                self.eat()?;
                if self.peek()?.kind == TokenKind::LParen {
                    self.eat()?;
                    let Token {
                        kind: TokenKind::Identifier(modifier),
                        span,
                    } = self.expect(&TokenKind::Identifier(String::new()))?
                    else {
                        unreachable!();
                    };
                    let modifier = if modifier == "parent" {
                        PropertyModifier::ParentPublic
                    } else if modifier == "child" {
                        PropertyModifier::ChildrenPublic
                    } else {
                        return Err(ParseError::UnexpectedToken(Token {
                            kind: TokenKind::Identifier(modifier),
                            span,
                        }));
                    };
                    self.expect(&TokenKind::RParen)?;
                    modifier
                } else {
                    PropertyModifier::Public
                }
            }
            _ => PropertyModifier::Private,
        };
        let curr = self.peek()?;
        match curr.kind {
            TokenKind::Identifier(_) => {
                let ty = self.parse_type()?;
                self.expect(&TokenKind::LBrace)?;
                let expr = self.parse_element_expr()?;
                self.expect(&TokenKind::RBrace)?;
                Ok(ElementDeffinition {
                    kind: ElementDeffinitionKind::Child(expr),
                    span: curr.span,
                })
            }
            TokenKind::Prop => {
                self.eat()?;
                let Token {
                    kind: TokenKind::Identifier(ident),
                    ..
                } = self.expect(&TokenKind::Identifier(String::new()))?
                else {
                    unreachable!()
                };
                match self.peek()?.kind {
                    TokenKind::SemiColon => {
                        span.end = self.eat()?.span.end;
                        Ok(ElementDeffinition {
                            kind: ElementDeffinitionKind::Property {
                                name: ident,
                                modifier,
                                ty: None,
                                rhs: None,
                            },
                            span,
                        })
                    }
                    TokenKind::Colon => {
                        self.eat()?;
                        let ty = self.parse_type()?;
                        let curr = self.eat()?;
                        let rhs = match curr.kind {
                            TokenKind::SemiColon => {
                                span.end = curr.span.end;
                                None
                            }
                            TokenKind::Eq => {
                                self.eat()?;
                                let expr = self.parse_expression(span);
                                span.end = self.expect(&TokenKind::SemiColon)?.span.end;
                                Some(expr)
                            }
                            _ => return Err(ParseError::UnexpectedToken(curr)),
                        };
                        Ok(ElementDeffinition {
                            kind: ElementDeffinitionKind::Property {
                                name: ident,
                                modifier,
                                ty: Some(ty),
                                rhs,
                            },
                            span,
                        })
                    }
                    TokenKind::Eq => {
                        self.eat()?;
                        let expr = self.parse_expression(span);
                        span.end = self.expect(&TokenKind::SemiColon)?.span.end;
                        Some(expr)
                    }
                    _ => return Err(ParseError::UnexpectedToken(self.eat()?)),
                }
            }
        }
    }
    ///Parses a component declaration. This initializes on the 'component' keyword
    pub(crate) fn parse_component(&mut self, mut span: Span) -> Result<ASTDeclaration, ParseError> {
        let ty = self.parse_type()?;
        self.expect(&TokenKind::LBrace)?;
        let mut defs = Vec::new();
        while self.peek()?.kind != TokenKind::RBrace {
            defs.push(self.parse_element_deffinition()?);
        }
        let Token { span: end, .. } = self.expect(&TokenKind::RBrace)?;
        span.end = end.end;
        Ok(ASTDeclaration {
            kind: ASTDeclarationKind::ElementDeclaration {
                name: ty,
                deffinitions: defs,
            },
            span,
        })
    }
}
