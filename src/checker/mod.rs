mod declarations;
pub mod error;
mod statments;
use std::{collections::HashMap, mem::discriminant};

use crate::{
    checker::error::{IncompatibleComponentReason, TypeError, TypeErrorKind},
    hir::{
        HirId, SlynxHir,
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind,
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
    ///Tracking of the types of the properties
    properties: HashMap<PropRef, HirType>,
}

impl TypeChecker {
    ///Checks the types of the provided `hir` and mutates them if needed. Any that could not be inferred but, yet is valid, is
    ///at the end, returned as it's default type
    pub fn check(hir: &mut SlynxHir) -> Result<(), TypeError> {
        let mut inner = Self {
            types: HashMap::new(),
            properties: HashMap::new(),
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

    fn substitute(&mut self, id: HirId, ty: HirType) {
        self.types.insert(id, ty);
    }

    ///Resolves recursively the names of the types. If A -> B, B -> int; then we assume that A -> int
    fn resolve(&self, ty: &HirType) -> Result<HirType, TypeError> {
        match ty {
            HirType::VarReference(rf) => {
                if let Some(ty) = self.types.get(rf) {
                    let resolved = self.resolve(ty)?;
                    Ok(resolved)
                } else {
                    Ok(ty.clone())
                }
            }
            HirType::Reference { rf, generics } => Ok(HirType::Reference {
                rf: *rf,
                generics: generics.clone(),
            }),
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

    ///Tries to unify types `a` and `b` if possible
    fn unify(&mut self, a: &HirType, b: &HirType, span: &Span) -> Result<HirType, TypeError> {
        let resolved_a = self.resolve(a)?;
        let resolved_b = self.resolve(b)?;

        match (&resolved_a, &resolved_b) {
            (out, HirType::Infer) | (HirType::Infer, out) => Ok(out.clone()),
            (aty, bty) if discriminant(aty) == discriminant(bty) => Ok(resolved_a),
            (HirType::VarReference(rf), b) | (b, HirType::VarReference(rf)) => {
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
                    });
                }
                let mut unified_props = Vec::with_capacity(aprops.len());
                for (prop_a, prop_b) in aprops.into_iter().zip(aprops.into_iter()) {
                    let unified_prop = self.unify(&prop_a.2, &prop_b.2, span)?;
                    unified_props.push((prop_a.0.clone(), prop_a.1.clone(), unified_prop));
                }
                Ok(HirType::Component {
                    props: unified_props,
                })
            }
            (t @ HirType::Component { .. }, HirType::GenericComponent)
            | (HirType::GenericComponent, t @ HirType::Component { .. }) => Ok(t.clone()),

            (t @ HirType::Reference { rf, generics }, HirType::GenericComponent { .. })
            | (HirType::GenericComponent, t @ HirType::Reference { rf, generics }) => {
                let referenced_type = self.types.get(rf).ok_or(TypeError {
                    kind: TypeErrorKind::Unrecognized(*rf),
                    span: span.clone(),
                })?;
                if let HirType::Component { props } = referenced_type {
                    let gen_count = generics.len();
                    let generic_count = props
                        .iter()
                        .filter(|prop| matches!(prop.2, HirType::Generic(_)))
                        .count();
                    if gen_count != generic_count {
                        Err(TypeError {
                            kind: TypeErrorKind::IncompatibleTypes {
                                lhs: referenced_type.clone(),
                                rhs: t.clone(),
                            },
                            span: span.clone(),
                        })
                    } else {
                        Ok(t.clone())
                    }
                } else {
                    Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: referenced_type.clone(),
                            rhs: HirType::GenericComponent,
                        },
                        span: span.clone(),
                    })
                }
            }
            (a, b) => {
                return Err(TypeError {
                    kind: TypeErrorKind::IncompatibleTypes {
                        lhs: a.clone(),
                        rhs: b.clone(),
                    },
                    span: span.clone(),
                });
            }
        }
    }

    fn unify_with_ref(
        &mut self,
        rf: HirId,
        ty: &HirType,
        span: &Span,
    ) -> Result<HirType, TypeError> {
        let resolved_ref = self.resolve(&HirType::VarReference(rf))?;
        if !matches!(resolved_ref, HirType::VarReference(_)) {
            return self.unify(&resolved_ref, ty, span);
        }
        if let HirType::VarReference(refe) = ty
            && rf == *refe
        {
            return Ok(HirType::Reference {
                rf: *refe,
                generics: Vec::new(),
            });
        }
        if self.reccursive_ty(rf, &ty) {
            return Err(TypeError {
                kind: TypeErrorKind::CiclicType { ty: ty.clone() },
                span: span.clone(),
            });
        }
        self.substitute(rf, ty.clone());
        Ok(HirType::VarReference(rf))
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

    fn default_expr(&mut self, expr: &mut HirExpression) -> Result<(), TypeError> {
        match expr.kind {
            HirExpressionKind::StringLiteral(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::Str, &expr.span)?
            }
            HirExpressionKind::Int(_) => {
                expr.ty = self.unify(&expr.ty, &HirType::Int, &expr.span)?
            }
            HirExpressionKind::Uint8x4(..) => {
                expr.ty = self.unify(&expr.ty, &HirType::Uint8x4, &expr.span)?
            }
            HirExpressionKind::Uint16x2(..) => {
                expr.ty = self.unify(&expr.ty, &HirType::Uint16x2, &expr.span)?
            }
            HirExpressionKind::Int8x4(..) => {
                expr.ty = self.unify(&expr.ty, &HirType::Int8x4, &expr.span)?
            }
            HirExpressionKind::Int16x2(..) => {
                expr.ty = self.unify(&expr.ty, &HirType::Int16x2, &expr.span)?
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
            HirExpressionKind::Element {
                ref name,
                ref mut values,
            } => {
                let HirType::Reference { rf, generics } = expr.ty.clone() else {
                    unreachable!("Element expression should be of type reference");
                };

                let HirType::Component { ref mut props } =
                    self.types.get_mut(&rf).cloned().ok_or(TypeError {
                        kind: TypeErrorKind::Unrecognized(rf),
                        span: expr.span.clone(),
                    })?
                else {
                    unreachable!("Reference type of an element should be a component");
                };

                for val in values {
                    match val {
                        ElementValueDeclaration::Property {
                            index, value, span, ..
                        } => {
                            if let Some(value) = value {
                                let ty = self.get_type_of_expr(value, span)?;
                                match props[*index].2 {
                                    HirType::Generic(idx) => {
                                        self.unify(&generics[idx], &ty, &span)?;
                                    }
                                    _ => {
                                        props[*index].2 =
                                            self.unify(&props[*index].2, &ty, &span)?
                                    }
                                }
                            }
                        }
                        ElementValueDeclaration::Js(_) => {} //since this shit is raw js, there's no way to know anything about it
                        ElementValueDeclaration::Child { name, values, span } => {
                            let HirType::Reference { rf, generics } = name else {
                                unreachable!("Type of child should be a reference")
                            };
                            let ty = self
                                .types
                                .get(rf)
                                .ok_or(TypeError {
                                    kind: TypeErrorKind::Unrecognized(*rf),
                                    span: span.clone(),
                                })?
                                .clone();
                            self.resolve_element_values(values, ty)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn default_statment(
        &mut self,
        statment: &mut HirStatment,
        expected: &HirType,
    ) -> Result<(), TypeError> {
        match statment.kind {
            HirStatmentKind::Expression { ref mut expr } => self.default_expr(expr)?,
            HirStatmentKind::Return { ref mut expr } => {
                self.default_expr(expr)?;

                let unify = self.unify(&expr.ty, &expected, &statment.span)?;
                expr.ty = unify;
            }
        };
        Ok(())
    }

    fn set_default(&mut self, decl: &mut HirDeclaration) -> Result<(), TypeError> {
        match decl.kind {
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
                    self.default_statment(statment, &return_type)?;
                }
            }
            HirDeclarationKind::ElementDeclaration { ref mut props } => {
                self.resolve_element_values(props, decl.ty.clone())?;
            }
        }
        Ok(())
    }
}
