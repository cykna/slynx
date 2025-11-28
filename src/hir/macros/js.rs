use crate::{
    ast::{ASTExpression, ASTStatment, ASTStatmentKind},
    hir::macros::StatmentMacro,
};

#[derive(Debug)]
///Macro for generating in js in place
pub struct JSMacro {}

impl StatmentMacro for JSMacro {
    fn name(&self) -> &'static str {
        "@js"
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
