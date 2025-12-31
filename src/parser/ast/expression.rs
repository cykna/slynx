use crate::parser::ast::{ComponentExpression, Operator, Span};

#[derive(Debug)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTExpressionKind {
    Element(ComponentExpression),
    IntLiteral(i32),
    StringLiteral(String),
    FloatLiteral(f32),
    Binary {
        lhs: Box<ASTExpression>,
        op: Operator,
        rhs: Box<ASTExpression>,
    },
    Identifier(String),
}
