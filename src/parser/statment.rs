use crate::parser::{
    Parser,
    ast::{ASTStatment, ASTStatmentKind, Span},
    error::ParseError,
    lexer::tokens::{Token, TokenKind},
};

impl Parser {
    ///Parses a let statment. Until now it's only for variable declaration, so, this only parses 'let name: t = value;' or 'let name = value;', same for mut variants
    ///Maybe, in the future, more things will be parsed.
    ///Obs: this function should initialize right after 'let' token, and the `letstan` the span of the 'let' token
    pub fn parse_let_statment(&mut self, letspan: Span) -> Result<ASTStatment, ParseError> {
        let mut mutable = false;
        if let TokenKind::Mut = self.peek()?.kind {
            self.eat()?;
            mutable = true;
        }
        let Token {
            kind: TokenKind::Identifier(name),
            ..
        } = self.expect(&TokenKind::Identifier("".into()))?
        else {
            unreachable!();
        };
        let vartype = match self.peek()?.kind {
            TokenKind::Colon => {
                self.eat()?;
                Some(self.parse_type()?)
            }
            _ => None,
        };
        self.eat()?; //eat '='
        let rhs = self.parse_expression()?;
        Ok(ASTStatment {
            span: Span {
                start: letspan.start,
                end: rhs.span.end,
            },
            kind: if mutable {
                ASTStatmentKind::MutableVar {
                    name: name,
                    ty: vartype,
                    rhs,
                }
            } else {
                ASTStatmentKind::Var {
                    name,
                    ty: vartype,
                    rhs,
                }
            },
        })
    }
    
    pub fn parse_statment(&mut self) -> Result<ASTStatment, ParseError> {
        match self.peek()?.kind {
            TokenKind::Let => {
                let span = self.eat()?.span;
                self.parse_let_statment(span)
            }
            _ => {
                let expr = self.parse_expression()?;
                if matches!(self.peek()?.kind, TokenKind::Eq) && expr.is_assignable() {
                    self.eat()?;
                    let rhs = self.parse_expression()?;
                    Ok(ASTStatment { 
                        span: Span {start: expr.span.start, end: rhs.span.end},
                        kind: ASTStatmentKind::Assign { lhs: expr, rhs: rhs }
                    })
                }else{
                    Ok(ASTStatment {
                        span: expr.span.clone(),
                        kind: ASTStatmentKind::Expression(expr),
                    })
                }
            }
        }
    }
}
