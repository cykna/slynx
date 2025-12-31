mod expression;
mod elements;
mod types;

pub use expression::*;
pub use types::*;
pub use elements::*;

#[derive(Debug, Clone,Eq,PartialEq)]
///The representation of the bounds of something on the code. 
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
///Some operator on the code. Something like, +, - , *, /, &, &&, etc
pub enum Operator {
    Add,
    Sub,
    Star,
    Slash,
}

#[derive(Debug)]
///Some statment on the code, a statment not necessarily have value, in general expressions do.
pub struct ASTStatment {
    pub kind: ASTStatmentKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTStatmentKind {
    Var {
        name: String,
        ty: Option<GenericIdentifier>,
        rhs: ASTExpression,
    },
    MutableVar {
        name: String,
        ty: Option<GenericIdentifier>,
        rhs: ASTExpression,
    },
    Expression(ASTExpression),
}

#[derive(Debug)]
pub struct ASTDeclaration {
    pub kind: ASTDeclarationKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct ObjectField {
    pub visibility: PropertyModifier,
    pub name: TypedName
}

#[derive(Debug)]
pub enum ASTDeclarationKind {
    ObjectDeclaration {
        name: GenericIdentifier,
        fields: Vec<ObjectField>
    },
    ComponentDeclaration {
        name: GenericIdentifier,
        members: Vec<ComponentMember>,
    },
    FuncDeclaration {
        name: GenericIdentifier,
        args: Vec<TypedName>,
        return_type: GenericIdentifier,
        body: Vec<ASTStatment>,
    },
}
