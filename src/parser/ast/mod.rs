mod expression;
mod macros;
mod elements;
mod types;

pub use expression::*;
pub use macros::*;
pub use types::*;
pub use elements::*;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub enum Operator {
    Add,
    Sub,
    Star,
    Slash,
}

#[derive(Debug)]
pub struct ASTStatment {
    pub kind: ASTStatmentKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTStatmentKind {
    MacroCall(MacroCallStmt),
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
pub enum ASTDeclarationKind {
    MacroCall(MacroCallDecl),
    ElementDeclaration {
        name: GenericIdentifier,
        deffinitions: Vec<ElementDeffinition>,
    },
    FuncDeclaration {
        name: GenericIdentifier,
        args: Vec<TypedName>,
        return_type: GenericIdentifier,
        body: Vec<ASTStatment>,
    },
}
