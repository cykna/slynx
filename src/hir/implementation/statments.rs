use crate::{
    hir::{
        SlynxHir,
        deffinitions::{HirStatment, HirStatmentKind},
        error::HIRError, types::HirType,
    },
    parser::ast::{ASTStatment, ASTStatmentKind},
};

impl SlynxHir {
    pub fn resolve_statment(&mut self, statment: ASTStatment) -> Result<HirStatment, HIRError> {
        match statment.kind {
            ASTStatmentKind::Expression(expr) => {
                let expr = self.resolve_expr(expr, None)?;
                Ok(HirStatment {
                    span: expr.span.clone(),
                    kind: HirStatmentKind::Expression { expr },
                })
            }
            ASTStatmentKind::MutableVar { name, ty, rhs } => {
                let ty = ty.map(|ty| self.retrieve_type_of_name(&ty, &statment.span)).transpose()?;
                
                let rhs = self.resolve_expr(rhs, ty.as_ref())?;
                let ty = ty.unwrap_or(HirType::Infer);
                let id = self.create_hirid_for(name, ty.clone());
                self.last_scope().set_mutable(id);
                Ok(HirStatment {
                    kind: HirStatmentKind::Variable { name: id, value: rhs, ty },
                    span: statment.span
                })
            }
            ASTStatmentKind::Var { name, ty, rhs } => {
                let ty = ty.map(|ty| self.retrieve_type_of_name(&ty, &statment.span)).transpose()?;
                let rhs = self.resolve_expr(rhs, ty.as_ref())?;
                let ty = ty.unwrap_or(HirType::Infer);
                let id = self.create_hirid_for(name, ty.clone());
                Ok(HirStatment {
                    kind: HirStatmentKind::Variable { name: id, value: rhs, ty },
                    span: statment.span
                })
            }
        }
    }
}
