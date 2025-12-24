use std::mem::discriminant;

use crate::{
    hir::{
        HirId, SlynxHir,
        declaration::{HirExpression, HirExpressionKind},
        error::HIRError,
        types::HirType,
    },
    parser::ast::{ASTExpression, ASTExpressionKind, Operator, Span},
};

impl SlynxHir {
    ///Ty only serves to tell the type of the expression if it's needed to infer and check if it doesnt correspond
    pub fn resolve_expr(
        &mut self,
        expr: ASTExpression,
        ty: Option<&HirType>,
    ) -> Result<HirExpression, HIRError> {
        match expr.kind {
            ASTExpressionKind::Binary { lhs, op, rhs } => self.resolve_binary(*lhs, op, *rhs, ty),
            ASTExpressionKind::StringLiteral(s) => Ok(HirExpression {
                id: HirId::new(),
                ty: HirType::Str,
                kind: HirExpressionKind::StringLiteral(s),
                span: expr.span,
            }),
            ASTExpressionKind::Identifier(name) => {
                let id = self.retrieve_information_of_scoped(&name, &expr.span)?;
                Ok(HirExpression {
                    kind: HirExpressionKind::Identifier(id),
                    id: HirId::new(),
                    ty: HirType::Reference {
                        rf: id,
                        generics: None,
                    },
                    span: expr.span,
                })
            }
            ASTExpressionKind::IntLiteral(int) => Ok(HirExpression {
                kind: HirExpressionKind::Int(int),
                ty: HirType::Int,
                id: HirId::new(),
                span: expr.span,
            }),

            ASTExpressionKind::FloatLiteral(float) => Ok(HirExpression::float(float, expr.span)),
            ASTExpressionKind::Element(element) => {
                let (id, ty) =
                    self.retrieve_information_of(&element.name.identifier, &element.span)?;

                Ok(HirExpression {
                    kind: HirExpressionKind::Element {
                        name: id,
                        values: self.resolve_element_values(element.values, &ty)?,
                    },
                    id: HirId::new(),
                    ty,
                    span: expr.span,
                })
            }
        }
    }
    pub fn resolve_binary(
        &mut self,
        lhs: ASTExpression,
        op: Operator,
        rhs: ASTExpression,
        ty: Option<&HirType>,
    ) -> Result<HirExpression, HIRError> {
        let mut lhs = self.resolve_expr(lhs, ty)?;
        let mut rhs = self.resolve_expr(rhs, ty)?;
        if discriminant(&lhs.ty) != discriminant(&rhs.ty) {
            if matches!(lhs.ty, HirType::Infer) {
                lhs.ty = rhs.ty.clone();
            } else if matches!(rhs.ty, HirType::Infer) {
                rhs.ty = lhs.ty.clone();
            }
        }
        let span = Span {
            start: lhs.span.start,
            end: lhs.span.end,
        };
        Ok(HirExpression {
            ty: lhs.ty.clone(),
            kind: HirExpressionKind::Binary {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            },
            id: HirId::new(),
            span,
        })
    }
}
