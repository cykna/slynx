use crate::{
    hir::macros::{ElementMacro, StatmentMacro},
    parser::ast::{
        ASTExpression, ASTExpressionKind, ASTStatment, ASTStatmentKind, ElementDeffinition,
        ElementDeffinitionKind, MacroElementArgs,
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
            MacroElementArgs::Statments(s) => {
                let mut out = Vec::new();
                for statment in s {
                    let ASTStatment {
                        span,
                        kind:
                            ASTStatmentKind::Expression(ASTExpression {
                                kind: ASTExpressionKind::StringLiteral(s),
                                ..
                            }),
                    } = statment
                    else {
                        unreachable!(
                            "Could not execute macro because statment {statment:?} wasnt a string literal"
                        )
                    };
                    out.push(ElementDeffinition {
                        kind: ElementDeffinitionKind::RawJs(s.clone().into()),
                        span: span.clone(),
                    });
                }

                out
            }
        }
    }
}
