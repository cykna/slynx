use crate::{
    hir::{HirId, types::HirType},
    parser::ast::{Operator, Span},
};

#[derive(Debug)]
pub enum SpecializedElement {
    Text {
        text: Box<HirExpression>,
    },
    Div {
        children: Vec<ElementValueDeclaration>,
    },
}

#[derive(Debug)]
#[repr(C)]
pub struct HirDeclaration {
    pub kind: HirDeclarationKind,
    pub id: HirId,
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug)]
#[repr(C)]
pub enum HirDeclarationKind {
    Function {
        statments: Vec<HirStatment>,
        name: String,
    },
    ElementDeclaration {
        props: Vec<ElementValueDeclaration>,
    },
}

#[derive(Debug)]
#[repr(C)]
pub enum ElementValueDeclaration {
    Property {
        id: HirId,
        ///The index of the property on the component
        index: usize,
        value: Option<HirExpression>,
        span: Span,
    },
    Child {
        name: HirId,
        values: Vec<ElementValueDeclaration>,
        span: Span,
    },
    Specialized(SpecializedElement),
}

#[derive(Debug)]
#[repr(C)]
pub struct HirStatment {
    pub kind: HirStatmentKind,
    pub span: Span,
}

#[derive(Debug)]
#[repr(C)]
pub enum HirStatmentKind {
    Expression { expr: HirExpression },
    Return { expr: HirExpression },
}

#[derive(Debug)]
#[repr(C)]
pub struct HirExpression {
    pub id: HirId,
    pub ty: HirType,
    pub kind: HirExpressionKind,
    pub span: Span,
}

impl HirExpression {
    pub fn float(f: f32, span: Span) -> Self {
        Self {
            id: HirId::new(),
            ty: HirType::Float,
            kind: HirExpressionKind::Float(f),
            span,
        }
    }
}

#[derive(Debug)]
#[repr(C)]
pub enum HirExpressionKind {
    Int(i32),
    StringLiteral(String),
    Float(f32),
    Binary {
        lhs: Box<HirExpression>,
        op: Operator,
        rhs: Box<HirExpression>,
    },
    Identifier(HirId),
    Specialized(SpecializedElement),
    Element {
        name: HirId,
        ///reference to a type
        values: Vec<ElementValueDeclaration>,
    },
}
impl HirExpression {
    ///Creates a int expression that must be infered.
    pub fn int(i: i32, span: Span) -> Self {
        Self {
            kind: HirExpressionKind::Int(i),
            id: HirId::new(),
            ty: HirType::Infer,
            span,
        }
    }
}
impl HirStatmentKind {
    ///Creates a int expression that must be infered.
    pub fn int(i: i32, span: Span) -> Self {
        Self::Expression {
            expr: HirExpression {
                kind: HirExpressionKind::Int(i),
                id: HirId::new(),
                ty: HirType::Infer,
                span,
            },
        }
    }
    ///Creates a float expression.
    pub fn float(float: f32, span: Span) -> Self {
        Self::Expression {
            expr: HirExpression {
                kind: HirExpressionKind::Float(float),
                id: HirId::new(),
                ty: HirType::Float,
                span,
            },
        }
    }
}
