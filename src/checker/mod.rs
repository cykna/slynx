pub mod error;
use std::collections::HashMap;

use color_eyre::eyre::Result;

use crate::{
    checker::error::{IncompatibleComponentReason, TypeError, TypeErrorKind},
    hir::{
        HirId, SlynxHir,
        deffinitions::{
            ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind, SpecializedComponent,
        },
        types::HirType,
    },
    parser::ast::Span,
};

#[derive(Hash, PartialEq, Eq, Debug)]
pub struct PropRef {
    ///The id of the owner of the property
    owner: HirId,
    ///It's index inside the owner
    index: usize,
}

#[derive(Debug)]
pub struct TypeChecker {
    ///The type of everything that is expected to have some
    types: HashMap<HirId, HirType>,
}

impl TypeChecker {
    ///Checks the types of the provided `hir` and mutates them if needed. Any that could not be inferred but, yet is valid, is
    ///at the end, returned as it's default type
    pub fn check(hir: &mut SlynxHir) -> Result<()> {
        let mut inner = Self {
            types: HashMap::new(),
        };
        for decl in &mut hir.declarations {
            inner.check_decl(decl)?;
        }
        //for the ones that couldn't be inferred, put their default
        for decl in &mut hir.declarations {
            inner.set_default(decl)?;
        }
        Ok(())
    }
    fn check_decl(&mut self, decl: &mut HirDeclaration) -> Result<()> {
        self.types.insert(decl.id, decl.ty.clone());
        match decl.kind {
            HirDeclarationKind::Function {
                ref mut statments, ..
            } => {
                self.resolve_statments(statments, &decl.ty)?;
            }
            HirDeclarationKind::Object => {
                self.types.insert(decl.id, decl.ty.clone());
            }

            HirDeclarationKind::ComponentDeclaration { ref mut props } => {
                for prop in props {
                    let HirType::Component { props } = &mut decl.ty else {
                        unreachable!("Component declaration should have type component");
                    };

                    match prop {
                        ComponentMemberDeclaration::Property {
                            index, value, span, ..
                        } => {
                            if let Some(value) = value {
                                let index = *index;
                                let ty = self.get_type_of_expr(value, span)?;
                                props[index].2 = self.unify(&props[index].2, &ty, span)?;
                            }
                        }
                        ComponentMemberDeclaration::Child { .. }
                        | ComponentMemberDeclaration::Specialized(_) => {}
                    }
                }
            }
        }
        Ok(())
    }

    fn substitute(&mut self, id: HirId, ty: HirType) {
        self.types.insert(id, ty);
    }

    ///Resolves recursively the names of the types. If A -> B, B -> int; then we assume that A -> int
    fn resolve(&self, ty: &HirType) -> Result<HirType> {
        match ty {
            HirType::Field(rf, idx) => {
                if let Some(ty) = self.types.get(rf) {
                    let ty = self.resolve(ty)?;
                    if let HirType::Struct { fields } = ty {
                        Ok(fields[*idx].clone())
                    } else {
                        Err(TypeError {
                            kind: TypeErrorKind::IncompatibleTypes {
                                expected: ty,
                                received: HirType::Struct { fields: Vec::new() },
                            },
                            span: Span { start: 0, end: 0 },
                        }
                        .into())
                    }
                } else {
                    unreachable!(
                        "Not implemented when a reference doenst point nothing. This is unreacheable because probably this wont be achieved never"
                    )
                }
            }
            HirType::Reference { rf, .. } => {
                if let Some(ty) = self.types.get(rf) {
                    let resolved = self.resolve(ty)?;
                    Ok(resolved)
                } else {
                    Ok(ty.clone())
                }
            }
            HirType::Component { props } => {
                let resolved_props = {
                    let mut tys = Vec::with_capacity(props.len());
                    for prop in props {
                        tys.push((prop.0.clone(), prop.1.clone(), self.resolve(&prop.2)?));
                    }
                    tys
                };
                Ok(HirType::Component {
                    props: resolved_props,
                })
            }

            _ => Ok(ty.clone()),
        }
    }

    #[inline]
    fn get_type_of_name(&self, name: &HirId, span: &Span) -> Result<HirType> {
        self.types
            .get(name)
            .ok_or(
                TypeError {
                    kind: TypeErrorKind::Unrecognized(*name),
                    span: span.clone(),
                }
                .into(),
            )
            .cloned()
    }

    ///Tries to unify types `a` and `b` if possible
    fn unify(&mut self, a: &HirType, b: &HirType, span: &Span) -> Result<HirType> {
        let a = self.resolve(a)?;
        let b = self.resolve(b)?;

        match (&a, &b) {
            (HirType::Int, HirType::Int)
            | (HirType::Float, HirType::Float)
            | (HirType::Str, HirType::Str) => Ok(a),
            (out, HirType::Infer) | (HirType::Infer, out) if !matches!(out, HirType::Infer) => {
                Ok(out.clone())
            }
            (HirType::Reference { rf, .. }, b) | (b, HirType::Reference { rf, .. }) => {
                self.unify_with_ref(*rf, b, span)
            }
            (HirType::Component { props: aprops }, HirType::Component { props: bprops }) => {
                if aprops.len() != bprops.len() {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleComponent {
                            reason: IncompatibleComponentReason::DifferentPropAmount {
                                rhs: aprops.len(),
                                lhs: bprops.len(),
                            },
                        },
                        span: span.clone(),
                    }
                    .into());
                }
                let mut unified_props = Vec::with_capacity(aprops.len());
                for (prop_a, prop_b) in aprops.iter().zip(aprops.iter()) {
                    let unified_prop = self.unify(&prop_a.2, &prop_b.2, span)?;
                    unified_props.push((prop_a.0.clone(), prop_a.1.clone(), unified_prop));
                }
                Ok(HirType::Component {
                    props: unified_props,
                })
            }
            (t @ HirType::Component { .. }, HirType::GenericComponent)
            | (HirType::GenericComponent, t @ HirType::Component { .. }) => Ok(t.clone()),

            (HirType::Struct { fields: f1 }, HirType::Struct { fields: f2 }) => {
                if f1.len() != f2.len() {
                    Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            expected: b,
                            received: a,
                        },
                        span: span.clone(),
                    }
                    .into())
                } else {
                    for idx in 0..f1.len() {
                        self.unify(&f1[idx], &f2[idx], span)?;
                    }
                    Ok(a)
                }
            }
            (_, _) => Err(TypeError {
                kind: TypeErrorKind::IncompatibleTypes {
                    expected: b,
                    received: a,
                },
                span: span.clone(),
            }
            .into()),
        }
    }

    fn unify_with_ref(&mut self, rf: HirId, ty: &HirType, span: &Span) -> Result<HirType> {
        let resolved_ref = self.resolve(&HirType::Reference {
            rf,
            generics: Vec::new(),
        })?;
        if !matches!(resolved_ref, HirType::Reference { .. }) {
            return self.unify(&resolved_ref, ty, span);
        }
        if let HirType::Reference { rf: refe, .. } = ty
            && rf == *refe
        {
            return Ok(HirType::Reference {
                rf: *refe,
                generics: Vec::new(),
            });
        }
        if self.reccursive_ty(rf, ty) {
            return Err(TypeError {
                kind: TypeErrorKind::CiclicType { ty: ty.clone() },
                span: span.clone(),
            }
            .into());
        }
        self.substitute(rf, ty.clone());
        Ok(HirType::Reference {
            rf,
            generics: Vec::new(),
        })
    }

    ///Checks if the provided `ty` is recursive
    fn reccursive_ty(&self, ty_ref: HirId, ty: &HirType) -> bool {
        match ty {
            HirType::Reference { rf, .. } => {
                if ty_ref == *rf {
                    true
                } else if let Some(resolved) = self.types.get(rf) {
                    self.reccursive_ty(ty_ref, resolved)
                } else {
                    false
                }
            }
            HirType::Component { props } => {
                props.iter().any(|prop| self.reccursive_ty(ty_ref, &prop.2))
            }
            _ => false,
        }
    }

    fn resolve_specialized(&mut self, _: &mut SpecializedComponent) -> Result<()> {
        Ok(())
    }

    fn resolve_statments(&mut self, statments: &mut Vec<HirStatment>, ty: &HirType) -> Result<()> {
        let HirType::Function { return_type, .. } = ty else {
            unreachable!();
        };
        for statment in statments {
            match &mut statment.kind {
                HirStatmentKind::Return { expr } => {
                    expr.ty = self.unify(&expr.ty, return_type, &statment.span)?;
                }
                HirStatmentKind::Expression { expr } => {
                    expr.ty = self.get_type_of_expr(expr, &expr.span.clone())?;
                }
            }
        }
        Ok(())
    }

    fn resolve_component_members(
        &mut self,
        values: &mut Vec<ComponentMemberDeclaration>,
        mut target: HirType,
    ) -> Result<HirType> {
        let HirType::Component { ref mut props } = target else {
            unreachable!(
                "The type received when resolving component values should be a component one"
            );
        };
        for value in values {
            match value {
                ComponentMemberDeclaration::Specialized(spec) => {
                    self.resolve_specialized(spec)?;
                }
                ComponentMemberDeclaration::Property {
                    index, value, span, ..
                } => {
                    if let Some(value) = value {
                        let ty = self.get_type_of_expr(value, span)?;
                        props[*index].2 = self.unify(&props[*index].2, &ty, span)?;
                    }
                }
                ComponentMemberDeclaration::Child { name, values, span } => {
                    let ty = self
                        .types
                        .get(name)
                        .ok_or(TypeError {
                            kind: TypeErrorKind::Unrecognized(*name),
                            span: span.clone(),
                        })?
                        .clone();
                    self.resolve_component_members(values, ty)?;
                }
            }
        }
        Ok(target)
    }

    fn resolve_object_types(&mut self, ty: HirType, fields: &mut [HirExpression]) -> Result<()> {
        let HirType::Struct { fields: fields_tys } = ty else {
            unreachable!("When resolving object types, a type 'struct' should be provided");
        };
        for (idx, f) in fields.iter_mut().enumerate() {
            f.ty = self.unify(&fields_tys[idx], &f.ty, &f.span)?;
        }
        Ok(())
    }

    ///Retrieves the type of the provided `expr`. Returns infer if it could not be inferred.
    fn get_type_of_expr(&mut self, expr: &mut HirExpression, span: &Span) -> Result<HirType> {
        let expected = expr.ty.clone();

        let calc = match expr.kind {
            HirExpressionKind::Int(_) => HirType::Int,
            HirExpressionKind::Float(_) => HirType::Float,
            HirExpressionKind::StringLiteral(_) => HirType::Str,
            HirExpressionKind::Binary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                let lhs_ty = self.get_type_of_expr(lhs, &lhs.span.clone())?;
                let rhs_ty = self.get_type_of_expr(rhs, &rhs.span.clone())?;
                self.unify(&lhs_ty, &rhs_ty, span)?
            }
            HirExpressionKind::Identifier(_) => self.resolve(&expr.ty)?,
            HirExpressionKind::Component {
                name,
                ref mut values,
            } => {
                let parent = self
                    .types
                    .get_mut(&name)
                    .ok_or(TypeError {
                        kind: TypeErrorKind::Unrecognized(name),
                        span: span.clone(),
                    })?
                    .clone();
                self.resolve_component_members(values, parent)?
            }
            HirExpressionKind::Object {
                name,
                ref mut fields,
            } => {
                let obj = self.get_type_of_name(&name, span)?;
                self.resolve_object_types(obj, fields)?;
                HirType::Reference {
                    rf: name,
                    generics: Vec::new(),
                }
            }
            ref un => {
                unimplemented!("{un:?}")
            }
        };
        let unified = self.unify(&expected, &calc, span)?;
        expr.ty = unified.clone();
        Ok(unified)
    }

    fn default_expr(&mut self, expr: &mut HirExpression) -> Result<()> {
        match expr.kind {
            HirExpressionKind::StringLiteral(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::Str, &expr.span)?
            }
            HirExpressionKind::Int(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::Int, &expr.span)?
            }
            HirExpressionKind::Float(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::Float, &expr.span)?
            }
            HirExpressionKind::Binary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                self.default_expr(rhs)?;
                self.default_expr(lhs)?;
                expr.ty = self.unify(&rhs.ty, &lhs.ty, &expr.span)?;
            }
            HirExpressionKind::Identifier(_) => {
                expr.ty = self.resolve(&expr.ty)?;
            }
            HirExpressionKind::Specialized(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::GenericComponent, &expr.span)?
            }
            HirExpressionKind::Object { .. } => {
                expr.ty = self.resolve(&expr.ty)?;
            }
            HirExpressionKind::FieldAccess {
                expr: ref mut parent,
                ..
            } => {
                parent.ty = self.resolve(&parent.ty)?;
                expr.ty = self.resolve(&expr.ty)?;
            }
            HirExpressionKind::Component {
                ref name,
                ref mut values,
            } => {
                let HirType::Component { ref mut props } = expr.ty.clone() else {
                    unreachable!("Component expression should be of type component");
                };
                expr.ty = HirType::Reference {
                    rf: *name,
                    generics: Vec::new(),
                };
                for val in values {
                    match val {
                        ComponentMemberDeclaration::Property {
                            index, value, span, ..
                        } => {
                            if let Some(value) = value {
                                let ty = self.get_type_of_expr(value, span)?;
                                props[*index].2 = self.unify(&props[*index].2, &ty, span)?;
                            }
                        }
                        ComponentMemberDeclaration::Specialized(_) => {}
                        ComponentMemberDeclaration::Child { name, values, span } => {
                            let ty = self
                                .types
                                .get(name)
                                .ok_or(TypeError {
                                    kind: TypeErrorKind::Unrecognized(*name),
                                    span: span.clone(),
                                })?
                                .clone();
                            self.resolve_component_members(values, ty)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn default_statment(&mut self, statment: &mut HirStatment, expected: &HirType) -> Result<()> {
        match statment.kind {
            HirStatmentKind::Expression { ref mut expr } => self.default_expr(expr)?,
            HirStatmentKind::Return { ref mut expr } => {
                self.default_expr(expr)?;
                let unify = self.unify(&expr.ty, expected, &statment.span)?;
                expr.ty = unify;
            }
        };
        Ok(())
    }

    fn set_default(&mut self, decl: &mut HirDeclaration) -> Result<()> {
        match decl.kind {
            HirDeclarationKind::Object => {}
            HirDeclarationKind::Function {
                ref mut statments, ..
            } => {
                let HirType::Function {
                    ref return_type, ..
                } = decl.ty
                else {
                    unreachable!("A function should have function type");
                };
                for statment in statments {
                    self.default_statment(statment, return_type)?;
                }
            }
            HirDeclarationKind::ComponentDeclaration { ref mut props } => {
                self.resolve_component_members(props, decl.ty.clone())?;
            }
        }
        Ok(())
    }
}
