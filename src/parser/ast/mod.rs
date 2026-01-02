mod component;
mod expression;
mod types;

pub use component::*;
pub use expression::*;
pub use types::*;

#[derive(Default, Debug, Clone)]
pub enum VisibilityModifier {
    ///Property visible to everyone
    Public,
    ///Property visible only by the one defining it.
    #[default]
    Private,
    ///Property visible only for the children. Only usable on Components.
    ChildrenPublic,
    ///Property visible only for the parents. Only usable on Components.
    ParentPublic,
}
#[derive(Debug, Clone, Eq, PartialEq)]
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
    pub visibility: VisibilityModifier,
    pub name: TypedName,
}

#[derive(Debug)]
pub enum ASTDeclarationKind {
    ObjectDeclaration {
        name: GenericIdentifier,
        fields: Vec<ObjectField>,
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
