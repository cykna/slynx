use crate::flattener::{expr::MaybeExpr, types::FlattenedType};

#[derive(Debug)]
pub enum ElementValueType {
    Property,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct ElementProperty {
    pub(crate) expr: MaybeExpr,
    pub(crate) ty: FlattenedType,
}

impl ElementProperty {
    pub fn new(expr: MaybeExpr, ty: FlattenedType) -> Self {
        Self { expr, ty }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union ElementValueUnion {
    pub(crate) property: ElementProperty,
}

#[repr(C)]
pub struct ElementValueFlattened {
    pub ty: ElementValueType,
    pub value: ElementValueUnion,
}

impl ElementValueFlattened {
    pub fn property(expr: MaybeExpr, ty: FlattenedType) -> Self {
        Self {
            ty: ElementValueType::Property,
            value: ElementValueUnion {
                property: ElementProperty::new(expr, ty),
            },
        }
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct ElementDef {
    pub(crate) defs: *const ElementValueFlattened,
    pub(crate) len: usize,
}
