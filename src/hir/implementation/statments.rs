use color_eyre::eyre::Result;

use crate::{
    hir::{
        SlynxHir,
        deffinitions::{HirStatment, HirStatmentKind},
    },
    parser::ast::{ASTExpression, ASTExpressionKind, ASTStatment, ASTStatmentKind},
};

impl SlynxHir {
    pub fn check_existance(&mut self, expr: &ASTExpression) -> Result<()> {
        match &expr.kind {
            ASTExpressionKind::FieldAccess { parent, .. } => {
                self.check_existance(parent)?;
            }
            ASTExpressionKind::Identifier(name) => {
                self.get_variable(name, &expr.span)?;
            }
            _ => {}
        }
        Ok(())
    }
    pub fn resolve_statment(&mut self, statment: ASTStatment) -> Result<HirStatment> {
        match statment.kind {
            ASTStatmentKind::Expression(expr) => {
                let expr = self.resolve_expr(expr, None)?;
                Ok(HirStatment {
                    span: expr.span.clone(),
                    kind: HirStatmentKind::Expression { expr },
                })
            }
            ASTStatmentKind::Assign { lhs, rhs } => {
                let lhs = self.resolve_expr(lhs, None)?;
                let rhs = self.resolve_expr(rhs, None)?;

                Ok(HirStatment {
                    kind: HirStatmentKind::Assign { lhs, value: rhs },
                    span: statment.span,
                })
            }
            ASTStatmentKind::MutableVar { name, ty, rhs } => {
                let typeid = ty.and_then(|t| {
                    self.retrieve_information_of_type(&t.identifier, &statment.span)
                        .ok()
                        .map(|inner| inner.0)
                });
                let rhs = self.resolve_expr(rhs, typeid)?;

                let id = self.create_variable(&name, rhs.ty, true);

                Ok(HirStatment {
                    kind: HirStatmentKind::Variable {
                        name: id,
                        value: rhs,
                    },
                    span: statment.span,
                })
            }
            ASTStatmentKind::Var { name, ty, rhs } => {
                let typeid = ty.and_then(|t| {
                    self.retrieve_information_of_type(&t.identifier, &statment.span)
                        .ok()
                        .map(|inner| inner.0)
                });
                let rhs = self.resolve_expr(rhs, typeid)?;

                let id = self.create_variable(&name, rhs.ty, false);

                Ok(HirStatment {
                    kind: HirStatmentKind::Variable {
                        name: id,
                        value: rhs,
                    },
                    span: statment.span,
                })
            }
        }
    }
}
