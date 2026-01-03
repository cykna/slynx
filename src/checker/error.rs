use crate::{
    hir::{HirId, types::HirType},
    parser::ast::Span,
};

#[derive(Debug)]
pub enum IncompatibleComponentReason {
    DifferentPropAmount { rhs: usize, lhs: usize },
}

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    CannotCastType {
        expected: HirType,
        received: HirType,
    },
    CiclicType {
        ty: HirType,
    },
    IncompatibleComponent {
        reason: IncompatibleComponentReason,
    },
    IncompatibleTypes {
        expected: HirType,
        received: HirType,
    },
    Unrecognized(HirId),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match &self.kind {
            TypeErrorKind::CannotCastType { expected, received } => {
                format!("Could not cast type '{expected:?}' into '{received:?}'")
            }
            TypeErrorKind::CiclicType { ty } => {
                format!("The type '{ty:?}' is cyclic and cannot exist without recursion")
            }
            TypeErrorKind::IncompatibleComponent { reason } => {
                format!("The component is incompatible because of '{reason:?}'")
            }
            TypeErrorKind::IncompatibleTypes { expected, received} => format!(
                "Incompatible types. Was expecting to receive type '{expected:?}' instead got type '{received:?}'"
            ),
            TypeErrorKind::Unrecognized(_) => "Tem que fazer".to_string(),
        };
        write!(f, "{out}")
    }
}
