#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum FlattenedTypeKind {
    Int,
    Decimal,
    Uint8x4,
    Int8x4,
    Uint16x2,
    Int16x2,
    GenericComponent,
    Vector,
    Complex,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct ComplexTypeDef {
    tys: *const usize,
    len: usize,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union FlattenedInternalType {
    primitive: (),
    pub(crate) complex_def: ComplexTypeDef,
    pub(crate) vector: usize,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct FlattenedType {
    pub(crate) kind: FlattenedTypeKind,
    pub(crate) ty: FlattenedInternalType,
}

impl FlattenedType {
    pub fn complex(vec: Vec<usize>) -> Self {
        Self {
            kind: FlattenedTypeKind::Vector,
            ty: FlattenedInternalType {
                complex_def: ComplexTypeDef {
                    len: vec.len(),
                    tys: vec.leak().as_ptr(),
                },
            },
        }
    }
    pub fn int() -> Self {
        Self {
            kind: FlattenedTypeKind::Int,
            ty: FlattenedInternalType { primitive: () },
        }
    }
    pub fn float() -> Self {
        Self {
            kind: FlattenedTypeKind::Decimal,
            ty: FlattenedInternalType { primitive: () },
        }
    }
    pub fn duplet(unsigned: bool) -> Self {
        Self {
            kind: if unsigned {
                FlattenedTypeKind::Uint16x2
            } else {
                FlattenedTypeKind::Int16x2
            },
            ty: FlattenedInternalType { primitive: () },
        }
    }
    pub fn quadruplet(unsigned: bool) -> Self {
        Self {
            kind: if unsigned {
                FlattenedTypeKind::Uint8x4
            } else {
                FlattenedTypeKind::Int8x4
            },
            ty: FlattenedInternalType { primitive: () },
        }
    }
    pub fn generic_component() -> Self {
        Self {
            kind: FlattenedTypeKind::GenericComponent,
            ty: FlattenedInternalType { primitive: () },
        }
    }
    pub fn index(&self) -> Option<usize> {
        Some(match self.kind {
            FlattenedTypeKind::Int => 0,
            FlattenedTypeKind::Decimal => 1,
            FlattenedTypeKind::Int8x4 => 2,
            FlattenedTypeKind::Uint8x4 => 3,
            FlattenedTypeKind::Int16x2 => 4,
            FlattenedTypeKind::Uint16x2 => 5,
            FlattenedTypeKind::GenericComponent => 6,
            _ => return None,
        })
    }
}
