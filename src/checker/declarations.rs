use crate::{
    checker::{
        TypeChecker,
        error::{TypeError, TypeErrorKind},
    },
    hir::{
        declaration::{ElementValueDeclaration, HirDeclaration, HirDeclarationKind},
        types::HirType,
    },
};

impl TypeChecker {
    pub fn check_decl(&mut self, decl: &mut HirDeclaration) -> Result<(), TypeError> {
        self.types.insert(decl.id, decl.ty.clone());
        match decl.kind {
            HirDeclarationKind::Function { ref statments, .. } => {}
            HirDeclarationKind::ElementDeclaration { ref mut props } => {
                for prop in props {
                    let HirType::Component { props } = &mut decl.ty else {
                        unreachable!("Element declaration should have type component");
                    };

                    match prop {
                        ElementValueDeclaration::Js(_) => {}
                        ElementValueDeclaration::Property {
                            index,
                            value,
                            span,
                            id,
                        } => {
                            if let Some(value) = value {
                                let index = *index;
                                let ty = self.get_type_of_expr(value, span)?;
                                props[index].2 = self.unify(&props[index].2, &ty, span)?;
                                self.types.insert(*id, props[index].2.clone());
                            }
                        }
                        ElementValueDeclaration::Child { .. } => {}
                    }
                }
            }
        }
        Ok(())
    }

    pub fn resolve_element_values(
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
}
