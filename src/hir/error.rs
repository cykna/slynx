use crate::{
    hir::{HirId, declaration::HirExpression},
    parser::ast::{ElementExpression, Span},
};

#[derive(Debug)]
pub struct HIRError {
    pub kind: HIRErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum HIRErrorKind {
    TypeNotRecognized(String),
    NameNotRecognized(String),
    IdNotRecognized(HirId),
    InvalidBinaryExpression {
        lhs: HirExpression,
        rhs: HirExpression,
    },
    PropertyNotVisible {
        prop_name: String,
    },
    InvalidChild {
        child: ElementExpression,
    },
    InvalidType {
        ty: String,
        reason: InvalidTypeReason,
    },
}

#[derive(Debug)]
pub enum InvalidTypeReason {
    MissingGeneric,
}
