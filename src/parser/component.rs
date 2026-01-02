use crate::parser::{
    ast::{
        ASTDeclaration, ASTDeclarationKind, ComponentMember, ComponentMemberKind,
        GenericIdentifier, Span, VisibilityModifier,
    },
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

use super::Parser;
impl Parser {
    fn parse_modifier(&mut self) -> Result<VisibilityModifier, ParseError> {
        Ok(match self.peek()?.kind {
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
                        VisibilityModifier::ParentPublic
                    } else if modifier == "child" {
                        VisibilityModifier::ChildrenPublic
                    } else {
                        return Err(ParseError::UnexpectedToken(
                            Token {
                                kind: TokenKind::Identifier(modifier),
                                span,
                            },
                            "child' or 'parent' to determine who will be able to access it"
                                .to_string(),
                        ));
                    };
                    self.expect(&TokenKind::RParen)?;
                    modifier
                } else {
                    VisibilityModifier::Public
                }
            }
            _ => VisibilityModifier::Private,
        })
    }

    ///Checks if the next tokens will be used for statments or not. For so, as the ones for component deffinitions are more limited, we simply check if the next ones aren't they.
    ///So something like `pub prop` is understood as a deffinition, so this will be false. The same with `H1<f32> {}` but not `H1<f32>::abc`
    fn check_for_statment(&self) -> Result<bool, ParseError> {
        let out = match self.peek()?.kind {
            TokenKind::Prop | TokenKind::Pub => false,
            TokenKind::Identifier(_) => match self.peek_at(1)?.kind {
                TokenKind::Lt => {
                    //advance as much as <> needed
                    let mut idx = 2;
                    {
                        let mut gt_amount = 1;
                        while gt_amount > 0 {
                            match self.peek_at(idx)?.kind {
                                TokenKind::Gt => gt_amount -= 1,
                                TokenKind::Lt => gt_amount += 1,
                                _ => {}
                            }
                            idx += 1
                        }
                    }

                    match self.peek_at(idx)?.kind {
                        TokenKind::LBrace => false,
                        _ => true,
                    }
                }
                TokenKind::LBrace => false,
                _ => true,
            },
            _ => true,
        };
        Ok(out)
    }

    fn parse_component_member(&mut self) -> Result<ComponentMember, ParseError> {
        let mut span = self.peek()?.span.clone();
        let modifier = self.parse_modifier()?;
        let curr = self.peek()?;
        match curr.kind {
            TokenKind::Identifier(_) => {
                let span = curr.span.clone();
                let expr = self.parse_component_expr()?;
                Ok(ComponentMember {
                    kind: ComponentMemberKind::Child(expr),
                    span,
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
                if ident == "children" {
                    let Token { span: end, .. } = self.expect(&TokenKind::SemiColon)?;
                    return Ok(ComponentMember {
                        kind: ComponentMemberKind::Property {
                            name: ident,
                            modifier,
                            ty: Some(GenericIdentifier {
                                identifier: "Vector".to_string(),
                                generic: Some(vec![GenericIdentifier {
                                    identifier: "Component".to_string(),
                                    generic: None,
                                    span: Span {
                                        end: end.end,
                                        start: span.start,
                                    },
                                }]),
                                span: Span {
                                    end: end.end,
                                    start: span.start,
                                },
                            }),
                            rhs: None,
                        },
                        span: Span {
                            end: end.end,
                            start: span.start,
                        },
                    });
                }

                match self.peek()?.kind {
                    TokenKind::SemiColon => {
                        span.end = self.eat()?.span.end;
                        Ok(ComponentMember {
                            kind: ComponentMemberKind::Property {
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
                                let expr = self.parse_expression()?;

                                span.end = self.expect(&TokenKind::SemiColon)?.span.end;
                                Some(expr)
                            }
                            _ => {
                                return Err(ParseError::UnexpectedToken(
                                    curr,
                                    "Expecing ';' to determine this property initialization is required by it's parent or '=' to give it a default value".to_string(),
                                ));
                            }
                        };
                        Ok(ComponentMember {
                            kind: ComponentMemberKind::Property {
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
                        let expr = self.parse_expression()?;
                        span.end = self.expect(&TokenKind::SemiColon)?.span.end;
                        Ok(ComponentMember {
                            kind: ComponentMemberKind::Property {
                                name: ident,
                                modifier,
                                ty: None,
                                rhs: Some(expr),
                            },
                            span,
                        })
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            self.eat()?,
                            "'=' or ':' to define the type of the property or a ';' to keep it to be initialized by it's parent".to_string(),
                        ));
                    }
                }
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    self.eat()?,
                    "'prop' a macro name or an identifier".to_string(),
                ));
            }
        }
    }
    ///Parses a component declaration. This initializes on the 'component' keyword
    pub(crate) fn parse_component(&mut self, mut span: Span) -> Result<ASTDeclaration, ParseError> {
        let ty = self.parse_type()?;
        self.expect(&TokenKind::LBrace)?;
        let mut defs = Vec::new();
        while self.peek()?.kind != TokenKind::RBrace {
            defs.push(self.parse_component_member()?);
        }
        let Token { span: end, .. } = self.expect(&TokenKind::RBrace)?;
        span.end = end.end;
        Ok(ASTDeclaration {
            kind: ASTDeclarationKind::ComponentDeclaration {
                name: ty,
                members: defs,
            },
            span,
        })
    }
}
