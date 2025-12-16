use crate::{hir::{HirId, SlynxHir, declaration::ElementValueDeclaration, error::HIRError, types::{HirType, HirValue, HirValueKind}}, parser::ast::{ElementDeffinition, ElementDeffinitionKind}};

impl SlynxHir {
    pub fn resolve_component_defs(
        &mut self,
        def: Vec<ElementDeffinition>,
    ) -> Result<Vec<ElementValueDeclaration>, HIRError> {
        let mut out = Vec::with_capacity(def.len());
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
                        self.retrieve_type_of_name(&ty, &ty.span)?
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
                        span: def.span,
                    });
                    self.last_scope().insert_named_value(
                        id,
                        name,
                        HirValue {
                            ty,
                            kind: HirValueKind::Property { modifier },
                        },
                    );
                    prop_idx += 1;
                }
                ElementDeffinitionKind::Child(child) => {
                    let (id, ty) =
                        self.retrieve_information_of(&child.name.identifier, &child.span)?;
                    let values = self.resolve_element_values(child.values, &ty)?;
                    out.push(ElementValueDeclaration::Child {
                        name: id,
                        values,
                        span: child.span,
                    })
                }
                _ => {}
            }
        }
        Ok(out)
    }
}