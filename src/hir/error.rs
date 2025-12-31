use crate::{
    hir::declaration::HirExpression,
    parser::ast::{ComponentExpression, Span},
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
    NameAlreadyDefined(String),
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
        child: ComponentExpression,
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
                format!(
                    "The name '{name}' is not recognized. Check if it exists or you wrote some typo"
                )
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
            HIRErrorKind::NameAlreadyDefined(name) => {
                format!("The name '{name}' was already defined before. Use a different name")
            }
            HIRErrorKind::MissingProperty { prop_name } => {
                format!("Property named as '{prop_name}' is required but wasn't provided")
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
