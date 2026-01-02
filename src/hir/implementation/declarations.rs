use crate::{
    hir::{
        HirId, SlynxHir,
        declaration::{ComponentMemberDeclaration, SpecializedComponent},
        error::{HIRError, HIRErrorKind},
        types::HirType,
    },
    parser::ast::{
        ComponentMember, ComponentMemberKind, ComponentMemberValue, GenericIdentifier, ObjectField,
        Span, TypedName,
    },
};

impl SlynxHir {
    pub fn resolve_object(
        &mut self,
        name: GenericIdentifier,
        fields: Vec<ObjectField>,
    ) -> Result<(), HIRError> {
        let fields = {
            let mut out = Vec::with_capacity(fields.len());
            for field in fields {
                out.push(self.retrieve_type_of_name(&field.name.kind, &field.name.span)?);
            }
            out
        };
        self.create_hirid_for(name.to_string(), HirType::Struct { fields });
        Ok(())
    }

    pub fn hoist_object(
        &mut self,
        name: &GenericIdentifier,
        _: &Vec<ObjectField>,
    ) -> Result<(), HIRError> {
        self.create_hirid_for(name.to_string(), HirType::Struct { fields: Vec::new() });

        Ok(())
    }

    pub fn hoist_function(
        &mut self,
        name: &GenericIdentifier,
        args: &Vec<TypedName>,
        return_type: &GenericIdentifier,
    ) -> Result<(), HIRError> {
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
            return_type: Box::new(self.retrieve_type_of_name(return_type, &return_type.span)?),
        };

        self.create_hirid_for(name.to_string(), func_ty);
        Ok(())
    }

    pub fn specialize_text(
        &mut self,
        values: Vec<ComponentMemberValue>,
        span: &Span,
    ) -> Result<SpecializedComponent, HIRError> {
        let mut text = None;
        for value in values {
            match value {
                ComponentMemberValue::Assign {
                    prop_name,
                    rhs,
                    span,
                } => match prop_name.as_str() {
                    "text" => text = Some(self.resolve_expr(rhs, None)?),
                    _ => {
                        return Err(HIRError {
                            kind: HIRErrorKind::TypeNotRecognized(prop_name),
                            span: span.clone(),
                        });
                    }
                },
                ComponentMemberValue::Child(e) => {
                    return Err(HIRError {
                        kind: HIRErrorKind::InvalidChild { child: Box::new(e) },
                        span: span.clone(),
                    });
                }
            }
        }
        if let Some(text) = text {
            Ok(SpecializedComponent::Text {
                text: Box::new(text),
            })
        } else {
            Err(HIRError {
                kind: HIRErrorKind::MissingProperty {
                    prop_name: String::from("text"),
                },
                span: span.clone(),
            })
        }
    }

    pub fn resolve_component_defs(
        &mut self,
        def: Vec<ComponentMember>,
    ) -> Result<Vec<ComponentMemberDeclaration>, HIRError> {
        let mut out = Vec::with_capacity(def.len());
        let mut prop_idx = 0;
        for def in def {
            match def.kind {
                ComponentMemberKind::Property { ty, rhs, name, .. } => {
                    let ty = if let Some(ty) = ty {
                        self.retrieve_type_of_name(&ty, &ty.span)?
                    } else {
                        HirType::Infer
                    };
                    let id = HirId::new();
                    out.push(ComponentMemberDeclaration::Property {
                        id,
                        index: prop_idx,
                        value: if let Some(rhs) = rhs {
                            Some(self.resolve_expr(rhs, Some(&ty))?)
                        } else {
                            None
                        },
                        span: def.span,
                    });
                    self.last_scope().insert_name(id, name);
                    prop_idx += 1;
                }
                ComponentMemberKind::Child(child) => {
                    match (&child.name.generic, child.name.identifier.as_str()) {
                        (None, "Text") => {
                            let text = self.specialize_text(child.values, &child.span)?;
                            out.push(ComponentMemberDeclaration::Specialized(text));
                        }
                        _ => {
                            let (id, ty) =
                                self.retrieve_information_of(&child.name.identifier, &child.span)?;
                            let values = self.resolve_component_members(child.values, &ty)?;
                            out.push(ComponentMemberDeclaration::Child {
                                name: id,
                                values,
                                span: child.span,
                            })
                        }
                    }
                }
            }
        }
        Ok(out)
    }
}
