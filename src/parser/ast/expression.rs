use crate::parser::ast::{ElementExpression, Operator, Span};

#[derive(Debug)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTExpressionKind {
    Element(ElementExpression),
    IntLiteral(i32),
    StringLiteral(String),
    FloatLiteral(f32),
    Uint8x4Literal(
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
    ),
    Int8x4Literal(
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
    ),
    Int16x2Literal(Box<ASTExpression>, Box<ASTExpression>),
    Uint16x2Literal(Box<ASTExpression>, Box<ASTExpression>),
    Binary {
        lhs: Box<ASTExpression>,
        op: Operator,
        rhs: Box<ASTExpression>,
    },
    Identifier(String),
}