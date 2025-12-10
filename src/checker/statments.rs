use crate::{
    checker::{
        TypeChecker,
        error::{TypeError, TypeErrorKind},
    },
    hir::{
        declaration::{HirExpression, HirExpressionKind},
        types::HirType,
    },
    parser::ast::Span,
};

impl TypeChecker {
    ///Retrieves the type of the provided `expr`. Returns infer if it could not be inferred.
    pub fn get_type_of_expr(
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
                ref name,
                ref mut values,
            } => {
                let HirType::Reference { rf, .. } = name else {
                    unreachable!("Type of child should be a reference")
                };
                let parent = self
                    .types
                    .get_mut(rf)
                    .ok_or(TypeError {
                        kind: TypeErrorKind::Unrecognized(*rf),
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
}
