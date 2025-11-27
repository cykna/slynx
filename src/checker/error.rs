use crate::{
    ast::Span,
    hir::{HirId, types::HirType},
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
        lhs: HirType,
        rhs: HirType,
    },
    Unrecognized(HirId),
}
