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
    MissingProperty {
        prop_name: String,
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

impl std::fmt::Display for HIRError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            HIRErrorKind::IdNotRecognized(id) => write!(f, "Id not recognized: {:?}", id),
            HIRErrorKind::NameNotRecognized(name) => write!(f, "Name not recognized: {:?}", name),
            HIRErrorKind::TypeNotRecognized(ty) => write!(f, "Type not recognized: {:?}", ty),
            HIRErrorKind::MissingProperty { prop_name } => write!(
                f,
                "Property '{prop_name}' is obligatory but wasn't provided"
            ),
            HIRErrorKind::InvalidBinaryExpression { lhs, rhs } => {
                write!(f, "Invalid binary expression: {:?} {:?}", lhs, rhs)
            }
            HIRErrorKind::PropertyNotVisible { prop_name } => {
                write!(f, "Property not visible: {:?}", prop_name)
            }
            HIRErrorKind::InvalidChild { child } => write!(f, "Invalid child: {:?}", child),

            HIRErrorKind::InvalidType { ty, reason } => {
                write!(f, "Invalid type '{ty}', due to: {reason:?}",)
            }
        }
    }
}

impl std::error::Error for HIRError {}
