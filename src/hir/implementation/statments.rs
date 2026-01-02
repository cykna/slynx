use crate::{
    hir::{
        SlynxHir,
        deffinitions::{HirStatment, HirStatmentKind},
        error::HIRError,
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
            _ => {
                unimplemented!("{:?}", statment)
            }
        }
    }
}
