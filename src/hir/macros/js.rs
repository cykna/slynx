use crate::{
    ast::{ASTDeclaration, ASTExpression, ASTStatment, ASTStatmentKind},
    hir::macros::{DeclarationMacro, StatmentMacro},
};

#[derive(Debug)]
///Macro for generating in js in place
pub struct JSMacro {}

impl DeclarationMacro for JSMacro {
    fn name(&self) -> &'static str {
        "js"
    }
    fn execute(
        &self,
        args: &Vec<crate::ast::ASTDeclaration>,
        declaration_index: usize,
    ) -> Vec<crate::ast::ASTDeclaration> {
        vec![ASTDeclaration {
            kind: crate::ast::ASTDeclarationKind::ElementDeclaration {
                name: declaration_index.to_string(),
                deffinitions: Vec::new(),
            },
            span: args[0].span.clone(),
        }]
    }
}

impl StatmentMacro for JSMacro {
    fn name(&self) -> &'static str {
        "js"
    }
    fn execute(
        &self,
        args: &Vec<crate::ast::ASTStatment>,
        statment_index: usize,
    ) -> Vec<crate::ast::ASTStatment> {
        vec![ASTStatment {
            kind: ASTStatmentKind::Expression(ASTExpression {
                kind: crate::ast::ASTExpressionKind::Identifier(statment_index.to_string()),
                span: args[0].span.clone(),
            }),
            span: args[0].span.clone(),
        }]
    }
}
