use crate::{
    hir::{
        HirId, SlynxHir,
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirStatment,
            HirStatmentKind,
        },
        error::HIRError,
        types::{HirType, HirValue, HirValueKind},
    },
    parser::ast::{
        ASTDeclaration, ASTDeclarationKind, ASTStatmentKind, ElementDeffinition,
        ElementDeffinitionKind, GenericIdentifier, PropertyModifier,
    },
};

impl SlynxHir {
    ///Resolves the deffinitions of a component declaration. The `def`are the deffinitions of the component and `ty`, the type of the component.
    fn resolve_component_defs(
        &mut self,
        def: Vec<ElementDeffinition>,
        ty: &HirType,
    ) -> Result<Vec<ElementValueDeclaration>, HIRError> {
        let mut out = Vec::with_capacity(def.len());
        let HirType::Component { props } = ty else {
            unreachable!();
        };
        let mut prop_idx = 0;
        for def in def {
            match def.kind {
                ElementDeffinitionKind::RawJs(js) => out.push(ElementValueDeclaration::Js(js)),
                ElementDeffinitionKind::Property {
                    modifier,
                    ty,
                    rhs,
                    name,
                } => {
                    let ty = if let Some(ty) = ty {
                        let ty = match self.retrieve_type_of_name(&ty, &ty.span) {
                            Ok(ty) => ty,
                            Err(_) => props[prop_idx].2.clone(),
                        };
                        ty
                    } else {
                        HirType::Infer
                    };
                    let id = HirId::new();

                    out.push(ElementValueDeclaration::Property {
                        id,
                        index: prop_idx,
                        value: if let Some(rhs) = rhs {
                            Some(self.resolve_expr(rhs, Some(&ty))?)
                        } else {
                            None
                        },
                        span: def.span.clone(),
                    });

                    self.last_scope().insert_named_value(
                        id,
                        name,
                        HirValue {
                            ty,
                            kind: HirValueKind::Property { modifier },
                        },
                        &def.span,
                    )?;
                    prop_idx += 1;
                }
                ElementDeffinitionKind::Child(child) => {
                    let (id, ty) =
                        self.retrieve_information_of(&child.name.identifier, &child.span)?;
                    let generic_types = if let Some(concretes) = child.name.generic {
                        concretes
                            .iter()
                            .map(|concrete| self.retrieve_type_of_name(concrete, &child.span))
                            .collect::<Result<_, _>>()?
                    } else {
                        Vec::new()
                    };
                    let values = self.resolve_element_values(child.values, &ty)?;
                    out.push(ElementValueDeclaration::Child {
                        name: HirType::Reference {
                            rf: id,
                            generics: generic_types,
                        },
                        values,
                        span: child.span,
                    })
                }
                _ => {}
            }
        }
        Ok(out)
    }

    ///Tries to return a generic type index. This is used mainly when there's something that is created via generics, and a field uses that generic type. This function
    ///tries to returns it's index on the generic type deffinitions.
    ///For example:
    ///```rs
    /// component A<B,C> {
    ///     pub prop value:B;
    ///     pub prop value2: T;
    ///```
    ///On finding the type 'B' this returns Some(0), on trying to find T, it returns None
    pub fn retrieve_generic_type_index(
        generics: &[GenericIdentifier],
        name: &str,
    ) -> Option<usize> {
        generics
            .iter()
            .position(|generic| generic.identifier == name)
    }

    ///Hoist the provided `ast` declaration, with so no errors of undefined values because declared later may occurr
    pub fn hoist(&mut self, ast: &ASTDeclaration) -> Result<(), HIRError> {
        match &ast.kind {
            ASTDeclarationKind::MacroCall(..) => {}
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                return_type,
                ..
            } => {
                let args = {
                    let mut vec = Vec::with_capacity(args.len());
                    for arg in args {
                        let ty = self.retrieve_type_of_name(&arg.kind, &arg.span)?;
                        vec.push(ty);
                    }
                    vec
                };
                let func_ty = HirType::Function {
                    args,
                    return_type: Box::new(
                        self.retrieve_type_of_name(&return_type, &return_type.span)?,
                    ),
                };
                self.create_hirid_for(
                    name.to_string(), //add suport for generic identifier
                    HirValue {
                        kind: HirValueKind::Function {
                            modifier: PropertyModifier::Private,
                        },
                        ty: func_ty.clone(),
                    },
                    func_ty,
                    &ast.span,
                )?;
            }
            ASTDeclarationKind::ElementDeclaration {
                name: el_name,
                deffinitions,
            } => {
                let props = {
                    let mut out = Vec::with_capacity(deffinitions.len());
                    for def in deffinitions {
                        match &def.kind {
                            ElementDeffinitionKind::Property {
                                name, modifier, ty, ..
                            } => {
                                let prop_type = if let Some(prop_ty) = ty {
                                    match self.retrieve_type_of_name(&prop_ty, &def.span) {
                                        Ok(ty) => ty,
                                        Err(e) => {
                                            if let Some(ref generic_defs) = el_name.generic
                                                && let Some(idx) = Self::retrieve_generic_type_index(
                                                    generic_defs,
                                                    &prop_ty.identifier,
                                                )
                                                && let None = prop_ty.generic
                                            {
                                                HirType::Generic(idx)
                                            } else {
                                                return Err(e);
                                            }
                                        }
                                    }
                                } else {
                                    HirType::Infer
                                };
                                out.push((modifier.clone(), name.clone(), prop_type));
                            }
                            ElementDeffinitionKind::Child(_) => {}
                            _ => {}
                        }
                    }
                    out
                };
                self.create_hirid_for(
                    el_name.identifier.clone(), //add support for generic identifier
                    HirValue {
                        kind: HirValueKind::Component {
                            modifier: PropertyModifier::Private,
                        },
                        ty: HirType::GenericComponent,
                    },
                    HirType::Component { props },
                    &ast.span,
                )?;
            }
        }
        Ok(())
    }
    pub fn resolve(&mut self, ast: ASTDeclaration) -> Result<(), HIRError> {
        match ast.kind {
            ASTDeclarationKind::MacroCall(..) => {}
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                mut body,
                ..
            } => {
                let (id, func) = self.retrieve_information_of(&name.identifier, &ast.span)?; //modify later to accept the generic identifier instead

                self.enter_scope();
                for arg in args {
                    let id = HirId::new();
                    let ty = self.retrieve_type_of_name(&arg.kind, &arg.kind.span)?;
                    self.last_scope().insert_named_value(
                        id,
                        arg.name,
                        HirValue {
                            kind: HirValueKind::Variable,
                            ty,
                        },
                        &arg.span,
                    )?;
                }
                let statments = if let Some(last) = body.pop() {
                    let mut statments = Vec::with_capacity(body.len());
                    for ast in body {
                        statments.push(self.resolve_statment(ast)?);
                    }
                    if let ASTStatmentKind::Expression(expr) = last.kind {
                        let expr = self.resolve_expr(expr, None)?;
                        statments.push(HirStatment {
                            span: expr.span.clone(),
                            kind: HirStatmentKind::Return { expr },
                        });
                    }
                    statments
                } else {
                    Vec::new()
                };

                self.declarations.push(HirDeclaration {
                    kind: HirDeclarationKind::Function {
                        statments,
                        name: name.to_string(),
                    },
                    id,
                    ty: func,
                    span: ast.span,
                });
                self.exit_scope();
            }
            ASTDeclarationKind::ElementDeclaration { deffinitions, name } => {
                self.enter_scope();

                let (hir, ty) = self.retrieve_information_of(&name.identifier, &ast.span)?; //modify later to accept the generic identifier instead

                let defs = self.resolve_component_defs(deffinitions, &ty)?;
                self.declarations.push(HirDeclaration {
                    id: hir,
                    kind: HirDeclarationKind::ElementDeclaration { props: defs },
                    ty,
                    span: ast.span,
                });
                self.exit_scope();
            }
        }
        Ok(())
    }
}
