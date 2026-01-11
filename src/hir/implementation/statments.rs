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
                self.last_scope().retrieve_name(name, &expr.span)?;
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
                let (ty, rhs) = if let Some(ty) = ty {
                    let ty = self.retrieve_type_of_name(&ty, &statment.span)?;
                    let rhs = self.resolve_expr(rhs, Some(&ty))?;
                    (ty, rhs)
                } else {
                    let rhs = self.resolve_expr(rhs, None)?;
                    (rhs.ty.clone(), rhs)
                };

                let id = self.create_hirid_for(name, ty.clone());
                self.last_scope().set_mutable(id);
                Ok(HirStatment {
                    kind: HirStatmentKind::Variable {
                        name: id,
                        value: rhs,
                        ty,
                    },
                    span: statment.span,
                })
            }
            ASTStatmentKind::Var { name, ty, rhs } => {
                let (ty, rhs) = if let Some(ty) = ty {
                    let ty = self.retrieve_type_of_name(&ty, &statment.span)?;
                    let rhs = self.resolve_expr(rhs, Some(&ty))?;
                    (ty, rhs)
                } else {
                    let rhs = self.resolve_expr(rhs, None)?;
                    (rhs.ty.clone(), rhs)
                };

                let id = self.create_hirid_for(name, ty.clone());
                Ok(HirStatment {
                    kind: HirStatmentKind::Variable {
                        name: id,
                        value: rhs,
                        ty,
                    },
                    span: statment.span,
                })
            }
        }
    }
}
