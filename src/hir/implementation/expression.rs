use std::mem::discriminant;

use crate::{
    hir::{
        HirId, SlynxHir,
        deffinitions::{HirExpression, HirExpressionKind},
        error::{HIRError, HIRErrorKind},
        types::HirType,
    },
    parser::ast::{ASTExpression, ASTExpressionKind, NamedExpr, Operator, Span},
};

impl SlynxHir {
    
    pub fn organized_object_fields(&mut self, objid:HirId, ty: &HirType, fields: Vec<NamedExpr>, span:&Span) -> Result<HirExpressionKind, HIRError>{
        let Some(defined_layout) = self.objects_deffinitions.get(&objid) else {
            
            unreachable!("The deffinition of this should have been defined during hoisting and the resolving of it")
        };
        if defined_layout.len() != fields.len() {
           if defined_layout.len() > fields.len() {
               let missing_fields = defined_layout.iter().filter_map(|field| {
                   if let Some(_) = fields.iter().position(|f| &f.name == field) {
                       None
                   }else {
                       Some(field.clone())
                   }
               }).collect::<Vec<String>>();
               return Err(HIRError { kind: HIRErrorKind::MissingProperty { prop_names: missing_fields }, span: span.clone() })
           }
        }
        Ok(HirExpressionKind::Object { name: HirId::new(), fields: Vec::new()})
    }
    
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
                        generics: Vec::new(),
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
            ASTExpressionKind::Component(component) => {
                let (id, ty) =
                    self.retrieve_information_of(&component.name.identifier, &component.span)?;

                Ok(HirExpression {
                    kind: HirExpressionKind::Component {
                        name: id,
                        values: self.resolve_component_members(component.values, &ty)?,
                    },
                    id: HirId::new(),
                    ty,
                    span: expr.span,
                })
            }
            ASTExpressionKind::ObjectExpression { name, fields } => {
                let (id, ty) = self.retrieve_information_of(&name.identifier, &expr.span)?;
                let kind = self.organized_object_fields(id, &ty, fields, &expr.span)?;
                Ok(HirExpression { id: HirId::new(), ty, kind, span: expr.span })
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
