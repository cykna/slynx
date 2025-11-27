use crate::ast::Operator;

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum ExpressionIdentifier {
    Int,
    Float,
    Identifier,
    Int8x4,
    Uint8x4,
    Int16x2,
    Uint16x2,
    Binary,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Douplet {
    ///The pointer inside the flattened array for the first
    pub a: u64,
    ///The pointer inside the flattened array for the second
    pub b: u64,
}
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct Quadruplet {
    ///The pointer inside the flattened array for the first
    pub a: u64,
    ///The pointer inside the flattened array for the second
    pub b: u64,
    ///The pointer inside the flattened array for the third
    pub c: u64,
    ///The pointer inside the flattened array for the fourth
    pub d: u64,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct BinaryExpression {
    pub(crate) operator: Operator,
    ///lhs pointer inside the flattened array
    pub(crate) lhs: u64,
    ///rhs pointer inside the flattened array
    pub(crate) rhs: u64,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub union ExpressionKind {
    pub(crate) int: i32,
    pub(crate) float: f32,
    pub(crate) identifier: u64,
    pub(crate) double: Douplet,
    pub(crate) quadruplet: Quadruplet,
    pub(crate) binary: BinaryExpression,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct Expression {
    pub(crate) identifier: ExpressionIdentifier,
    pub(crate) kind: ExpressionKind,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct MaybeExpr {
    pub(crate) is_valued: u8,
    pub(crate) expr: u64,
}

impl MaybeExpr {
    pub fn new(id: u64) -> Self {
        Self {
            is_valued: 1,
            expr: id,
        }
    }
    pub fn empty() -> Self {
        Self {
            is_valued: 0,
            expr: 0,
        }
    }
}

impl Expression {
    pub fn int(int: i32) -> Self {
        Self {
            identifier: ExpressionIdentifier::Int,
            kind: ExpressionKind { int },
        }
    }
    pub fn float(float: f32) -> Self {
        Self {
            identifier: ExpressionIdentifier::Float,
            kind: ExpressionKind { float },
        }
    }
    pub fn identifier(id: u64) -> Self {
        Self {
            identifier: ExpressionIdentifier::Identifier,
            kind: ExpressionKind { identifier: id },
        }
    }

    pub fn duplet(a: u64, b: u64, unsigned: bool) -> Self {
        Self {
            identifier: if unsigned {
                ExpressionIdentifier::Uint16x2
            } else {
                ExpressionIdentifier::Int16x2
            },
            kind: ExpressionKind {
                double: Douplet { a, b },
            },
        }
    }
    pub fn quadruplet(a: u64, b: u64, c: u64, d: u64, unsigned: bool) -> Self {
        Self {
            identifier: if unsigned {
                ExpressionIdentifier::Uint16x2
            } else {
                ExpressionIdentifier::Int16x2
            },
            kind: ExpressionKind {
                quadruplet: Quadruplet { a, b, c, d },
            },
        }
    }
    pub fn binary(lhs: u64, rhs: u64, op: Operator) -> Self {
        Self {
            identifier: ExpressionIdentifier::Binary,
            kind: ExpressionKind {
                binary: BinaryExpression {
                    operator: op,
                    lhs,
                    rhs,
                },
            },
        }
    }
}
