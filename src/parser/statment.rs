use crate::parser::{
    Parser,
    ast::{ASTStatment, ASTStatmentKind},
    error::ParseError,
};

impl Parser {
    pub fn parse_statment(&mut self) -> Result<ASTStatment, ParseError> {
        let expr = self.parse_expression()?;
        Ok(ASTStatment {
            span: expr.span.clone(),
            kind: ASTStatmentKind::Expression(expr),
        })
    }
}
