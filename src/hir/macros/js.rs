use crate::{
    hir::macros::{ElementMacro, StatmentMacro},
    parser::ast::{
        ASTExpression, ASTExpressionKind, ASTStatment, ASTStatmentKind, ElementDeffinition,
        ElementDeffinitionKind, MacroElementArgs, Span,
    },
};

#[derive(Debug)]
///Macro for generating in js in place
pub struct JSMacro {}

impl StatmentMacro for JSMacro {
    fn name(&self) -> &'static str {
        "js"
    }
    fn execute(&self, args: &Vec<ASTStatment>, statment_index: usize) -> Vec<ASTStatment> {
        vec![ASTStatment {
            kind: ASTStatmentKind::Expression(ASTExpression {
                kind: ASTExpressionKind::Identifier(statment_index.to_string()),
                span: args[0].span.clone(),
            }),
            span: args[0].span.clone(),
        }]
    }
}

impl ElementMacro for JSMacro {
    fn name(&self) -> &'static str {
        "js"
    }
    fn execute(&self, args: &MacroElementArgs, _: usize) -> Vec<ElementDeffinition> {
        match args {
            MacroElementArgs::Empty | MacroElementArgs::Deffinitions(_) => {
                vec![]
            }
            MacroElementArgs::Statments(_) => vec![ElementDeffinition {
                kind: ElementDeffinitionKind::RawJs("console.log('Hello world')".into()),
                span: Span { start: 0, end: 0 },
            }],
        }
    }
}
