use crate::{
    hir::{
        HirId,
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind,
        },
        types::HirType,
    },
    intermediate::monomorphizer::Monomorphizer,
};

impl Monomorphizer {
    ///Tries to monomorphize the provided `expr`
    pub fn monomorphize_expr(&mut self, expr: &mut HirExpression) {
        match &mut expr.kind {
            HirExpressionKind::Float(_)
            | HirExpressionKind::Int(_)
            | HirExpressionKind::StringLiteral(_)
            | HirExpressionKind::Identifier(_) => {}
            HirExpressionKind::Uint8x4(a, b, c, d) | HirExpressionKind::Int8x4(a, b, c, d) => {
                self.monomorphize_expr(a);
                self.monomorphize_expr(b);
                self.monomorphize_expr(c);
                self.monomorphize_expr(d);
            }
            HirExpressionKind::Binary { lhs, rhs, .. } => {
                self.monomorphize_expr(lhs);
                self.monomorphize_expr(rhs);
            }
            HirExpressionKind::Int16x2(a, b) | HirExpressionKind::Uint16x2(a, b) => {
                self.monomorphize_expr(a);
                self.monomorphize_expr(b);
            }
            HirExpressionKind::Element { name, values } => {
                let HirType::Reference { rf, generics } = &expr.ty else {
                    unreachable!("Type of element should be a reference")
                };
                let element = self
                    .base_elements
                    .get(rf)
                    .cloned()
                    .expect("Type of eleemnt reference should be known to be monomorphized");

                let props = if let HirDeclarationKind::ElementDeclaration { props } = element.kind {
                    let mut out = Vec::with_capacity(props.len());
                    for prop in props {
                        let decl = match prop {
                            ElementValueDeclaration::Js(js) => {
                                ElementValueDeclaration::Js(js.clone())
                            }
                            ElementValueDeclaration::Property {
                                index, value, span, ..
                            } => {
                                let expr = if let Some(expr) = value {
                                    let mut expr = expr.clone();
                                    self.monomorphize_expr(&mut expr);
                                    Some(expr)
                                } else {
                                    None
                                };

                                let prop_id = HirId::new();
                                ElementValueDeclaration::Property {
                                    id: prop_id,
                                    index,
                                    value: expr,
                                    span: span.clone(),
                                }
                            }
                            ElementValueDeclaration::Child { .. } => {
                                todo!(
                                    "Must modify element value declaration childs to support generics as well"
                                );
                            }
                        };
                        out.push(decl);
                    }

                    out
                } else {
                    unreachable!()
                };
                let tys = if let HirType::Component { props: tys } = element.ty {
                    tys.iter()
                        .map(|ty| {
                            if let HirType::Generic(idx) = ty.2 {
                                (ty.0.clone(), ty.1.clone(), generics[idx].clone())
                            } else {
                                ty.clone()
                            }
                        })
                        .collect::<Vec<_>>()
                } else {
                    unreachable!()
                };

                let new_id = HirId::new();
                let monomorphized = {
                    HirDeclaration {
                        kind: HirDeclarationKind::ElementDeclaration { props },
                        id: new_id,
                        ty: HirType::Component { props: tys },
                        span: expr.span.clone(),
                    }
                };
                expr.ty = monomorphized.ty.clone();
                self.elements.push(monomorphized);
                *name = new_id;
            }
        }
    }
}
