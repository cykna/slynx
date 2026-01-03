use crate::parser::{
    Parser,
    ast::{
        ASTExpression, ASTExpressionKind, ComponentExpression, ComponentMemberValue,
        GenericIdentifier, NamedExpr, Operator, Span,
    },
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

impl Parser {
    ///Parses an component expression but, starting from the LBrace, assuming the name of the component is the provided `name`
    pub fn parse_component_expr_with_name(
        &mut self,
        name: GenericIdentifier,
    ) -> Result<ComponentExpression, ParseError> {
        let mut span = Span {
            start: name.span.start,
            end: 0,
        };
        self.expect(&TokenKind::LBrace)?;
        let mut values = Vec::new();
        loop {
            if let Ok(curr) = self.peek()
                && curr.kind == TokenKind::RBrace
            {
                span.end = curr.span.end;
                break;
            };

            match self.peek_at(1)?.kind {
                TokenKind::Colon => {
                    let Token {
                        kind: TokenKind::Identifier(ident),
                        span,
                    } = self.expect(&TokenKind::Identifier(String::new()))?
                    else {
                        unreachable!();
                    };
                    self.expect(&TokenKind::Colon)?;
                    let val = self.parse_expression()?;
                    values.push(ComponentMemberValue::Assign {
                        prop_name: ident,
                        span: Span {
                            start: span.start,
                            end: val.span.end,
                        },
                        rhs: val,
                    });
                }
                _ => {
                    let val = self.parse_component_expr()?;
                    values.push(ComponentMemberValue::Child(val));
                }
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(ComponentExpression { name, values, span })
    }
    pub fn parse_component_expr(&mut self) -> Result<ComponentExpression, ParseError> {
        let ty = self.parse_type()?;
        self.parse_component_expr_with_name(ty)
    }

    pub fn parse_named_expr(&mut self) -> Result<NamedExpr, ParseError> {
        let Token {
            kind: TokenKind::Identifier(name),
            span: start,
        } = self.expect(&TokenKind::Identifier(String::new()))?
        else {
            unreachable!()
        };
        self.expect(&TokenKind::Colon)?;
        let expr = self.parse_expression()?;
        Ok(NamedExpr {
            span: Span {
                start: start.start,
                end: expr.span.end,
            },
            name,
            expr,
        })
    }

    pub fn parse_object_expression(&mut self) -> Result<ASTExpression, ParseError> {
        let name = self.parse_type()?;
        self.expect(&TokenKind::LParen)?;
        let mut fields = Vec::new();
        while self.peek()?.kind != TokenKind::RParen {
            let named_expr = self.parse_named_expr()?;
            fields.push(named_expr);
            if let TokenKind::RParen = self.peek()?.kind {
                break;
            } else {
                self.expect(&TokenKind::Comma)?;
            }
        }
        let Token { span: end, .. } = self.expect(&TokenKind::RParen)?;
        Ok(ASTExpression {
            span: Span {
                start: name.span.start,
                end: end.end,
            },
            kind: ASTExpressionKind::ObjectExpression { name, fields },
        })
    }

    ///Parses anything that comes withn identifier. This can be a function call, object creation, or a struct creation. This is executed without eating the identifier to be able to choose what to
    ///return
    pub fn parse_identifier_exprs(&mut self) -> Result<Option<ASTExpression>, ParseError> {
        let after_identifier = &self.peek_at(1)?.kind;
        match after_identifier {
            TokenKind::Lt => {
                let ty = self.parse_type()?;
                if let TokenKind::LBrace = self.peek()?.kind {
                    let component = self.parse_component_expr_with_name(ty)?;
                    Ok(Some(ASTExpression {
                        span: component.span.clone(),
                        kind: ASTExpressionKind::Component(component),
                    }))
                } else {
                    Err(ParseError::UnexpectedToken(self.eat()?, "'{'".to_string()))
                }
            }
            TokenKind::LBrace => {
                let component = self.parse_component_expr()?;
                Ok(Some(ASTExpression {
                    span: component.span.clone(),
                    kind: ASTExpressionKind::Component(component),
                }))
            }
            TokenKind::LParen => {
                match (&self.peek_at(2)?.kind, &self.peek_at(3)?.kind) {
                    //check if its name(a,b) or name(a:b), or name(.a:b)
                    (TokenKind::Identifier(_), TokenKind::Colon) => {
                        Ok(Some(self.parse_object_expression()?))
                    }

                    _ => Ok(None),
                }
            }
            _ => Ok(None),
        }
    }

    pub fn parse_primary(&mut self) -> Result<ASTExpression, ParseError> {
        let expr = if let TokenKind::Identifier(_) = self.peek()?.kind
            && let Some(value) = self.parse_identifier_exprs()?
        {
            value
        } else {
            let current = self.eat()?;
            match current.kind {
                TokenKind::Float(f) => Ok(ASTExpression {
                    kind: ASTExpressionKind::FloatLiteral(f),
                    span: current.span,
                }),
                TokenKind::Int(i) => Ok(ASTExpression {
                    kind: ASTExpressionKind::IntLiteral(i),
                    span: current.span,
                }),
                TokenKind::Identifier(i) => Ok(ASTExpression {
                    kind: ASTExpressionKind::Identifier(i),
                    span: current.span,
                }),
                TokenKind::String(s) => Ok(ASTExpression {
                    kind: ASTExpressionKind::StringLiteral(s),
                    span: current.span,
                }),
                TokenKind::LParen => {
                    let expr = self.parse_expression()?;
                    self.expect(&TokenKind::RParen)?;
                    Ok(expr)
                }

                _ => Err(ParseError::UnexpectedToken(
                    current,
                    "an expression".to_string(),
                )),
            }?
        };
        if let TokenKind::Dot = self.peek()?.kind {
            self.eat()?;
            self.parse_dot_postfix(expr)
        } else {
            Ok(expr)
        }
    }

    pub fn parse_multiplicative(&mut self) -> Result<ASTExpression, ParseError> {
        let mut lhs = self.parse_primary()?;
        while let Ok(curr) = self.peek()
            && matches!(curr.kind, TokenKind::Star | TokenKind::Slash)
        {
            let op = if let TokenKind::Star = self.eat()?.kind {
                Operator::Star
            } else {
                Operator::Slash
            };
            let rhs = self.parse_primary()?;
            lhs = ASTExpression {
                span: Span {
                    start: lhs.span.start,
                    end: rhs.span.end,
                },
                kind: ASTExpressionKind::Binary {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }
    pub fn parse_additive(&mut self) -> Result<ASTExpression, ParseError> {
        let mut lhs = self.parse_multiplicative()?;
        while let Ok(curr) = self.peek()
            && matches!(curr.kind, TokenKind::Plus | TokenKind::Sub)
        {
            let op = if let TokenKind::Plus = self.eat()?.kind {
                Operator::Add
            } else {
                Operator::Sub
            };
            let rhs = self.parse_multiplicative()?;
            lhs = ASTExpression {
                span: Span {
                    start: lhs.span.start,
                    end: rhs.span.end,
                },
                kind: ASTExpressionKind::Binary {
                    lhs: Box::new(lhs),
                    op,
                    rhs: Box::new(rhs),
                },
            };
        }
        Ok(lhs)
    }

    ///Parses a postfix that comes after a '.'. This function initializes right after the '.'
    pub fn parse_dot_postfix(
        &mut self,
        prefix: ASTExpression,
    ) -> Result<ASTExpression, ParseError> {
        let current = self.eat()?;
        match current.kind {
            TokenKind::Identifier(field) => Ok(ASTExpression {
                span: Span {
                    start: prefix.span.start,
                    end: current.span.end,
                },
                kind: ASTExpressionKind::FieldAccess {
                    parent: Box::new(prefix),
                    field,
                },
            }),
            _ => Err(ParseError::UnexpectedToken(
                current,
                "A field access".to_string(),
            )),
        }
    }

    pub fn parse_expression(&mut self) -> Result<ASTExpression, ParseError> {
        let expr = self.parse_additive()?;
        Ok(expr)
    }
}
