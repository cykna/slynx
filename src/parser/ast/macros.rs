use crate::parser::ast::{ASTDeclaration, ASTStatment, ElementDeffinition};

#[derive(Debug)]
pub struct MacroCallDecl {
    pub name: String,
    pub args: Vec<ASTDeclaration>,
}
#[derive(Debug)]
pub struct MacroCallStmt {
    pub name: String,
    pub args: Vec<ASTStatment>,
}

#[derive(Debug)]
///Arguments for a macro when being called inside a element deffinition.
///This is able to receive statments because it might be able to create functions with the
///provided ones
pub enum MacroElementArgs {
    Empty,
    Statments(Vec<ASTStatment>),
    Deffinitions(Vec<ElementDeffinition>),
}

#[derive(Debug)]
pub struct MacroCallElement {
    pub name: String,
    pub args: MacroElementArgs,
}

