use color_eyre::eyre::Result;

use crate::{
    hir::{
        SlynxHir,
        deffinitions::{HirStatment, HirStatmentKind},
    },
    parser::ast::{ASTStatment, ASTStatmentKind},
};

impl SlynxHir {
    pub fn resolve_statment(&mut self, statment: ASTStatment) -> Result<HirStatment> {
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
