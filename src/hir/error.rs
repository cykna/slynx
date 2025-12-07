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

impl std::fmt::Display for HIRError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match &self.kind {
            HIRErrorKind::NameNotRecognized(name) => {
                format!("Name '{name}' was not defined")
            }
            HIRErrorKind::TypeNotRecognized(name) => {
                format!("Type with name '{name}' is was not defined previously")
            }
            HIRErrorKind::InvalidBinaryExpression { .. } => {
                format!("Invalid binary expression")
            }
            HIRErrorKind::PropertyNotVisible { prop_name } => {
                format!("Property with name '{prop_name}' is not visible")
            }
            HIRErrorKind::InvalidChild { .. } => {
                format!("Invalid child. Component is not expecting children")
            }
            HIRErrorKind::InvalidType { ty, reason } => {
                format!("Invalid type '{ty}' because it's {reason}")
            }
        };
        write!(f, "{out}")
    }
}

#[derive(Debug)]
pub enum InvalidTypeReason {
    MissingGeneric,
}

impl std::fmt::Display for InvalidTypeReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InvalidTypeReason::MissingGeneric => write!(f, "missing generic type"),
        }
    }
}
