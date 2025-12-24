use crate::parser::{
    Parser,
    ast::{
        ASTExpression, ASTExpressionKind, ElementExpression, ElementValue, GenericIdentifier,
        Operator, Span,
    },
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

impl Parser {
    ///Parses an element expression but, starting from the LBrace, assuming the name of the element is the provided `name`
    pub fn parse_element_expr_with_name(
        &mut self,
        name: GenericIdentifier,
    ) -> Result<ElementExpression, ParseError> {
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
                    values.push(ElementValue::Assign {
                        prop_name: ident,
                        span: Span {
                            start: span.start,
                            end: val.span.end,
                        },
                        rhs: val,
                    });
                }
                _ => {
                    let val = self.parse_element_expr()?;
                    values.push(ElementValue::Element(val));
                }
            }
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(ElementExpression { name, values, span })
    }
    pub fn parse_element_expr(&mut self) -> Result<ElementExpression, ParseError> {
        let ty = self.parse_type()?;
        self.parse_element_expr_with_name(ty)
    }

    pub fn parse_primary(&mut self) -> Result<ASTExpression, ParseError> {
        match self.peek()?.kind {
            TokenKind::Identifier(_) => {
                let current_kind = &self.peek_at(1)?.kind;

                if matches!(current_kind, TokenKind::Lt) {
                    let ty = self.parse_type()?;
                    return if let TokenKind::LBrace = self.peek()?.kind {
                        let element = self.parse_element_expr_with_name(ty)?;
                        Ok(ASTExpression {
                            span: element.span.clone(),
                            kind: ASTExpressionKind::Element(element),
                        })
                    } else {
                        Err(ParseError::UnexpectedToken(self.eat()?, "'{'".to_string()))
                    };
                } else if matches!(current_kind, TokenKind::LBrace) {
                    let element = self.parse_element_expr()?;
                    return Ok(ASTExpression {
                        span: element.span.clone(),
                        kind: ASTExpressionKind::Element(element),
                    });
                } else {
                }
            }
            _ => {}
        };
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
        }
    }

    pub fn parse_multiplicative(&mut self) -> Result<ASTExpression, ParseError> {
        let mut lhs = self.parse_primary()?;
        while let Ok(curr) = self.peek()
            && matches!(curr.kind, TokenKind::Plus | TokenKind::Sub)
        {
            let op = if let TokenKind::Plus = self.eat()?.kind {
                Operator::Add
            } else {
                Operator::Sub
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

    pub fn parse_expression(&mut self) -> Result<ASTExpression, ParseError> {
        self.parse_additive()
    }
}
