pub mod error;
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
    fn check_decl(&mut self, decl: &mut HirDeclaration) -> Result<(), TypeError> {
        self.types.insert(decl.id, decl.ty.clone());
        match decl.kind {
            HirDeclarationKind::Function { .. } => {}
            HirDeclarationKind::ElementDeclaration { ref mut props } => {
                for prop in props {
                    let HirType::Component { props } = &mut decl.ty else {
                        unreachable!("Element declaration should have type component");
                    };

                    match prop {
                        ElementValueDeclaration::Js(_) => {}
                        ElementValueDeclaration::Property {
                            index, value, span, ..
                        } => {
                            if let Some(value) = value {
                                let index = *index;
                                let ty = self.get_type_of_expr(value, span)?;
                                props[index].2 = self.unify(&props[index].2, &ty, span)?;
                            }
                        }
                        ElementValueDeclaration::Child { .. } => {}
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
    fn resolve(&self, ty: &HirType) -> Result<HirType, TypeError> {
        match ty {
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

    ///Tries to unify types `a` and `b` if possible
    fn unify(&mut self, a: &HirType, b: &HirType, span: &Span) -> Result<HirType, TypeError> {
        let a = self.resolve(a)?;
        let b = self.resolve(b)?;

        match (&a, &b) {
            (out, HirType::Infer) | (HirType::Infer, out) => Ok(out.clone()),
            (aty, bty) if discriminant(aty) == discriminant(bty) => Ok(a),
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
        if self.reccursive_ty(rf, &ty) {
            return Err(TypeError {
                kind: TypeErrorKind::CiclicType { ty: ty.clone() },
                span: span.clone(),
            });
        }
        self.substitute(rf, ty.clone());
        Ok(HirType::Reference {
            rf: rf,
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

    fn resolve_element_values(
        &mut self,
        values: &mut Vec<ElementValueDeclaration>,
        mut target: HirType,
    ) -> Result<HirType, TypeError> {
        let HirType::Component { ref mut props } = target else {
            unreachable!(
                "The type received when resolving element values should be a component one"
            );
        };
        for value in values {
            match value {
                ElementValueDeclaration::Js(_) => {}
                ElementValueDeclaration::Property {
                    index, value, span, ..
                } => {
                    if let Some(value) = value {
                        let ty = self.get_type_of_expr(value, span)?;
                        props[*index].2 = self.unify(&props[*index].2, &ty, span)?;
                    }
                }
                ElementValueDeclaration::Child { name, values, span } => {
                    let ty = self
                        .types
                        .get(name)
                        .ok_or(TypeError {
                            kind: TypeErrorKind::Unrecognized(*name),
                            span: span.clone(),
                        })?
                        .clone();
                    self.resolve_element_values(values, ty)?;
                }
            }
        }
        Ok(target)
    }

    ///Retrieves the type of the provided `expr`. Returns infer if it could not be inferred.
    fn get_type_of_expr(
        &mut self,
        expr: &mut HirExpression,
        span: &Span,
    ) -> Result<HirType, TypeError> {
        let expected = expr.ty.clone();

        let calc = match expr.kind {
            HirExpressionKind::Int(_) => HirType::Int,
            HirExpressionKind::Int8x4(ref mut a, ref mut b, ref mut c, ref mut d) => {
                let a = self.get_type_of_expr(a, span)?;
                if !matches!(a, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: a,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let b = self.get_type_of_expr(b, span)?;
                if !matches!(b, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: b,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let c = self.get_type_of_expr(c, span)?;
                if !matches!(c, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: c,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let d = self.get_type_of_expr(d, span)?;
                if !matches!(d, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: d,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                HirType::Int8x4
            }
            HirExpressionKind::Uint8x4(ref mut a, ref mut b, ref mut c, ref mut d) => {
                let a = self.get_type_of_expr(a, span)?;
                if !matches!(a, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: a,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let b = self.get_type_of_expr(b, span)?;
                if !matches!(b, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: b,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let c = self.get_type_of_expr(c, span)?;
                if !matches!(c, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: c,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let d = self.get_type_of_expr(d, span)?;
                if !matches!(d, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: d,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                HirType::Uint8x4
            }
            HirExpressionKind::Int16x2(ref mut a, ref mut b) => {
                let a = self.get_type_of_expr(a, span)?;
                if !matches!(a, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: a,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let b = self.get_type_of_expr(b, span)?;
                if !matches!(b, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: b,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                HirType::Int16x2
            }
            HirExpressionKind::Uint16x2(ref mut a, ref mut b) => {
                let a = self.get_type_of_expr(a, &a.span.clone())?;
                if !matches!(a, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: a,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                let b = self.get_type_of_expr(b, &b.span.clone())?;
                if !matches!(b, HirType::Int) {
                    return Err(TypeError {
                        kind: TypeErrorKind::IncompatibleTypes {
                            lhs: b,
                            rhs: HirType::Int,
                        },
                        span: span.clone(),
                    });
                }
                HirType::Uint16x2
            }
            HirExpressionKind::Float(_) => HirType::Float,
            HirExpressionKind::StringLiteral(_) => HirType::Str,
            HirExpressionKind::Binary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                let lhs_ty = self.get_type_of_expr(lhs, &lhs.span.clone())?;
                let rhs_ty = self.get_type_of_expr(rhs, &rhs.span.clone())?;
                let ty = self.unify(&lhs_ty, &rhs_ty, span)?;
                ty
            }
            HirExpressionKind::Identifier(_) => self.resolve(&expr.ty)?,

            HirExpressionKind::Element {
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
                self.resolve_element_values(values, parent)?
            }
            ref un => {
                unimplemented!("{un:?}")
            }
        };
        let unified = self.unify(&expected, &calc, span)?;
        expr.ty = unified.clone();
        return Ok(unified);
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
                let HirType::Component { ref mut props } = expr.ty.clone() else {
                    unreachable!("Element expression should be of type element");
                };
                expr.ty = HirType::Reference {
                    rf: *name,
                    generics: Vec::new(),
                };
                for val in values {
                    match val {
                        ElementValueDeclaration::Property {
                            index, value, span, ..
                        } => {
                            if let Some(value) = value {
                                let ty = self.get_type_of_expr(value, span)?;
                                props[*index].2 = self.unify(&props[*index].2, &ty, &span)?;
                            }
                        }
                        ElementValueDeclaration::Js(_) => {} //since this shit is raw js, there's no way to know anything about it
                        ElementValueDeclaration::Child { name, values, span } => {
                            let ty = self
                                .types
                                .get(name)
                                .ok_or(TypeError {
                                    kind: TypeErrorKind::Unrecognized(*name),
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
