use crate::{
    hir::declaration::HirExpression,
    parser::ast::{ComponentExpression, Span},
};

#[derive(Debug)]
#[warn(unused)]
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
        lhs: Box<HirExpression>,
        rhs: Box<HirExpression>,
    },
    MissingProperty {
        prop_name: String,
    },
    PropertyNotVisible {
        prop_name: String,
    },
    InvalidChild {
        child: Box<ComponentExpression>,
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
            HIRErrorKind::InvalidBinaryExpression { .. } => "Invalid binary expression".to_string(),
            HIRErrorKind::PropertyNotVisible { prop_name } => {
                format!("Property with name '{prop_name}' is not visible")
            }
            HIRErrorKind::InvalidChild { .. } => {
                "Invalid child. Component is not expecting children".to_string()
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
