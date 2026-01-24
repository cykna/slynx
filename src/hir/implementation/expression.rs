use std::mem::discriminant;

use color_eyre::eyre::Result;

use crate::{
    hir::{
        DeclarationId, ExpressionId, SlynxHir, TypeId,
        deffinitions::{HirExpression, HirExpressionKind},
        error::{HIRError, HIRErrorKind},
        types::{FieldMethod, HirType},
    },
    parser::ast::{ASTExpression, ASTExpressionKind, NamedExpr, Operator, Span},
};

impl SlynxHir {
    pub fn organized_object_fields(
        &mut self,
        objid: DeclarationId,
        ty: TypeId,
        fields: Vec<NamedExpr>,
        span: &Span,
    ) -> Result<HirExpressionKind> {
        let HirType::Struct {
            fields: field_types,
        } = self.types_module.get_type(&ty)
        else {
            unreachable!("Type when organizing object fields should be a struct");
        };
        let Some(defined_layout) = self.declarations_module.retrieve_object_body(objid) else {
            unreachable!(
                "The deffinition of this should have been defined during hoisting and the resolving of it"
            )
        };
        if defined_layout.len() != fields.len() {
            if defined_layout.len() > fields.len() {
                let missing_fields = defined_layout
                    .iter()
                    .filter_map(|field| {
                        if fields
                            .iter()
                            .any(|f| self.symbols_module.intern(&f.name) == *field)
                        {
                            None
                        } else {
                            Some(self.symbols_module[*field].to_string())
                        }
                    })
                    .collect::<Vec<_>>();
                return Err(HIRError {
                    kind: HIRErrorKind::MissingProperty {
                        prop_names: missing_fields,
                    },
                    span: span.clone(),
                }
                .into());
            } else {
                let non_existent_fields = fields
                    .into_iter()
                    .filter(|provided_field| {
                        !defined_layout
                            .iter()
                            .any(|f| *f == self.symbols_module.intern(&provided_field.name))
                    })
                    .map(|f| f.name)
                    .collect::<Vec<String>>();
                return Err(HIRError {
                    kind: HIRErrorKind::PropertyNotRecognized {
                        prop_names: non_existent_fields,
                    },
                    span: span.clone(),
                }
                .into());
            }
        }
        let mut out = Vec::with_capacity(fields.len());
        let mut non_recognized_fields = Vec::with_capacity(fields.len());
        let cloned_layout = defined_layout.clone();
        for field in fields {
            if let Some(field_idx) = cloned_layout
                .iter()
                .position(|defined_field| &field.name == defined_field)
            {
                out.insert(
                    field_idx.max(out.len()),
                    self.resolve_expr(field.expr, Some(&field_types[field_idx]))?,
                );
            } else {
                non_recognized_fields.push(field.name);
            }
        }
        if non_recognized_fields.is_empty() {
            Ok(HirExpressionKind::Object {
                name: objid,
                fields: out,
            })
        } else {
            Err(HIRError {
                kind: HIRErrorKind::PropertyNotRecognized {
                    prop_names: non_recognized_fields,
                },
                span: span.clone(),
            }
            .into())
        }
    }

    /// Ty only serves to tell the type of the expression if it's needed to infer and check if it doesnt correspond
    pub fn resolve_expr(
        &mut self,
        expr: ASTExpression,
        ty: Option<TypeId>,
    ) -> Result<HirExpression> {
        match expr.kind {
            ASTExpressionKind::Binary { lhs, op, rhs } => self.resolve_binary(*lhs, op, *rhs, ty),
            ASTExpressionKind::StringLiteral(s) => Ok(HirExpression {
                id: ExpressionId::new(), // Changed to ExpressionId
                ty: self.types_module.str_id(),
                kind: HirExpressionKind::StringLiteral(s),
                span: expr.span,
            }),
            ASTExpressionKind::Identifier(name) => {
                let id = *self.get_variable(&name, &expr.span)?;
                let tyid = self
                    .types_module
                    .insert_variable(id, HirType::VarReference(id));
                Ok(HirExpression {
                    kind: HirExpressionKind::Identifier(id),
                    id: ExpressionId::new(), // Changed to ExpressionId
                    ty: tyid,
                    span: expr.span,
                })
            }
            ASTExpressionKind::IntLiteral(int) => Ok(HirExpression {
                kind: HirExpressionKind::Int(int),
                ty: self.types_module.int_id(),
                id: ExpressionId::new(), // Changed to ExpressionId
                span: expr.span,
            }),

            ASTExpressionKind::FloatLiteral(float) => Ok(HirExpression::float(float, expr.span)),
            ASTExpressionKind::Component(component) => {
                let (id, ty) =
                    self.retrieve_information_of_type(&component.name.identifier, &component.span)?;

                Ok(HirExpression {
                    kind: HirExpressionKind::Component {
                        name: id,
                        values: self.resolve_component_members(component.values, id)?,
                    },
                    id: ExpressionId::new(), // Changed to ExpressionId
                    ty: id,
                    span: expr.span,
                })
            }
            ASTExpressionKind::ObjectExpression { name, fields } => {
                let (id, ty) = self.retrieve_information_of_type(&name.identifier, &expr.span)?;
                let kind = self.organized_object_fields(id, &ty, fields, &expr.span)?;
                Ok(HirExpression {
                    id: ExpressionId::new(), // Changed to ExpressionId
                    ty: HirType::Reference {
                        rf: id,
                        generics: Vec::new(),
                    },
                    kind,
                    span: expr.span,
                })
            }
            ASTExpressionKind::FieldAccess { parent, field } => {
                let parent = self.resolve_expr(*parent, None)?;
                let HirExpression { ref ty, .. } = parent;
                match ty {
                    HirType::Reference { rf, .. } => {
                        if let Some(index) = self
                            .objects_deffinitions
                            .get(rf)
                            .expect("Object should have been defined")
                            .iter()
                            .position(|struct_field| &field == struct_field)
                        {
                            Ok(HirExpression {
                                id: ExpressionId::new(), // Changed to ExpressionId
                                ty: HirType::Field(FieldMethod::Type(*rf, index)),
                                kind: HirExpressionKind::FieldAccess {
                                    expr: Box::new(parent),
                                    field_index: index,
                                },
                                span: expr.span,
                            })
                        } else {
                            Err(HIRError {
                                kind: HIRErrorKind::PropertyNotRecognized {
                                    prop_names: vec![field],
                                },
                                span: expr.span,
                            }
                            .into())
                        }
                    }
                    HirType::VarReference(rf) => Ok(HirExpression {
                        id: ExpressionId::new(), // Changed to ExpressionId
                        ty: HirType::Field(FieldMethod::Variable(*rf, field)),
                        kind: HirExpressionKind::FieldAccess {
                            expr: Box::new(parent),
                            field_index: usize::MAX,
                        },
                        span: expr.span,
                    }),
                    u => unreachable!("{u:?}"),
                }
            }
        }
    }
    pub fn resolve_binary(
        &mut self,
        lhs: ASTExpression,
        op: Operator,
        rhs: ASTExpression,
        ty: Option<TypeId>,
    ) -> Result<HirExpression> {
        let mut lhs = self.resolve_expr(lhs, ty)?;
        let mut rhs = self.resolve_expr(rhs, ty)?;
        if discriminant(&lhs.ty) != discriminant(&rhs.ty) {
            if lhs.ty == self.types_module.infer_id() {
                lhs.ty = rhs.ty.clone();
            } else if rhs.ty == self.types_module.infer_id() {
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
            id: ExpressionId::new(), // Changed to ExpressionId
            span,
        })
    }
    /// Creates an int expression that must be inferred.
    pub fn create_intexpr(i: i32, span: Span, float) -> Self {
        Self {
            kind: HirExpressionKind::Int(i),
            id: ExpressionId::new(), // Changed from HirId::new()
            ty: HirType::infer_id(),
            span,
        }
    }

    /// Creates a float expression.
    pub fn create_floatexpr(float: f32, span: Span) -> Self {
        Self::Expression {
            expr: HirExpression {
                kind: HirExpressionKind::Float(float),
                id: ExpressionId::new(), // Changed from HirId::new()
                ty: HirType::float_id(),
                span,
            },
        }
    }
}
