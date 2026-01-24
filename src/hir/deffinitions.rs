use crate::{
    hir::{DeclarationId, ExpressionId, PropertyId, TypeId, VariableId, types::HirType},
    parser::ast::{Operator, Span},
};

#[derive(Debug)]
pub enum SpecializedComponent {
    Text {
        text: Box<HirExpression>,
    },
    Div {
        children: Vec<ComponentMemberDeclaration>,
    },
}

#[derive(Debug)]
#[repr(C)]
pub struct HirDeclaration {
    pub kind: HirDeclarationKind,
    pub id: DeclarationId,  // Changed from HirId
    pub ty: HirType,
    pub span: Span,
}

#[derive(Debug)]
#[repr(C)]
pub enum HirDeclarationKind {
    Object,
    Function {
        statments: Vec<HirStatment>,
        args: Vec<VariableId>,  // Changed from HirId - function arguments are variables
        name: String,
    },
    ComponentDeclaration {
        props: Vec<ComponentMemberDeclaration>,
    },
}

#[derive(Debug)]
#[repr(C)]
pub enum ComponentMemberDeclaration {
    Property {
        id: PropertyId,  // Changed from HirId
        /// The index of the property on the component
        index: usize,
        value: Option<HirExpression>,
        span: Span,
    },
    Child {
        name: TypeId,  // Still HirId - reference to a component declaration
        values: Vec<ComponentMemberDeclaration>,
        span: Span,
    },
    Specialized(SpecializedComponent),
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
    Assign {
        lhs: HirExpression,
        value: HirExpression,
    },
    Variable {
        name: VariableId,  // Changed from HirId
        value: HirExpression,
        ty: HirType,
    },
    Expression {
        expr: HirExpression,
    },
    Return {
        expr: HirExpression,
    },
}

#[derive(Debug)]
#[repr(C)]
pub struct HirExpression {
    pub id: ExpressionId,  // Changed from HirId
    pub ty: HirType,
    pub kind: HirExpressionKind,
    pub span: Span,
}

impl HirExpression {
    pub fn float(f: f32, span: Span) -> Self {
        Self {
            id: ExpressionId::new(),  // Changed from HirId::new()
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
    Identifier(VariableId),  // Still HirId - reference to a variable or declaration
    Specialized(SpecializedComponent),
    Component {
        name: TypeId,  // Still HirId - reference to a component type
        values: Vec<ComponentMemberDeclaration>,
    },
    Object {
        name: TypeId,  // Still HirId - reference to an object type
        fields: Vec<HirExpression>,
    },
    FieldAccess {
        expr: Box<HirExpression>,
        field_index: usize,
    },
}

impl HirExpression {
    /// Creates an int expression that must be inferred.
    pub fn int(i: i32, span: Span) -> Self {
        Self {
            kind: HirExpressionKind::Int(i),
            id: ExpressionId::new(),  // Changed from HirId::new()
            ty: HirType::Infer,
            span,
        }
    }
}

impl HirStatmentKind {
    /// Creates an int expression that must be inferred.
    pub fn int(i: i32, span: Span) -> Self {
        Self::Expression {
            expr: HirExpression {
                kind: HirExpressionKind::Int(i),
                id: ExpressionId::new(),  // Changed from HirId::new()
                ty: HirType::Infer,
                span,
            },
        }
    }
    
    /// Creates a float expression.
    pub fn float(float: f32, span: Span) -> Self {
        Self::Expression {
            expr: HirExpression {
                kind: HirExpressionKind::Float(float),
                id: ExpressionId::new(),  // Changed from HirId::new()
                ty: HirType::Float,
                span,
            },
        }
    }
}