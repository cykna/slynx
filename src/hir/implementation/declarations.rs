use color_eyre::eyre::Result;

use crate::hir::{
    HirId, SlynxHir,
    deffinitions::{
        ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, SpecializedComponent,
    },
    error::{HIRError, HIRErrorKind},
    types::HirType,
};
use crate::parser::ast::{
    ComponentMember, ComponentMemberKind, ComponentMemberValue, GenericIdentifier, ObjectField,
    Span, TypedName,
};

impl SlynxHir {
    pub fn resolve_object(
        &mut self,
        name: GenericIdentifier,
        fields: Vec<ObjectField>,
        span: Span,
    ) -> Result<()> {
        let mut fields = {
            let mut out = Vec::with_capacity(fields.len());
            for field in &fields {
                if field.name.kind.to_string() == name.to_string() {
                    return Err(HIRError {
                        kind: HIRErrorKind::RecursiveType {
                            ty: name.to_string(),
                        },
                        span: field.name.span.clone(),
                    }
                    .into());
                }
                out.push(self.retrieve_type_of_name(&field.name.kind, &field.name.span)?);
            }
            out
        };
        let id = self.retrieve_hirdid_of(&name.to_string(), &name.span)?;
        let HirType::Struct { fields: ty_field } =
            self.retrieve_ref_to_type(&name.identifier, &name.span)?
        else {
            unreachable!("WTF. Type of object should be a Struct ty");
        };

        ty_field.append(&mut fields);
        let ty = HirType::Struct {
            fields: ty_field.clone(),
        };
        self.declarations.push(HirDeclaration {
            kind: HirDeclarationKind::Object,
            id,
            ty,
            span,
        });

        Ok(())
    }

    pub fn hoist_object(
        &mut self,
        name: &GenericIdentifier,
        obj_fields: &[ObjectField],
    ) -> Result<()> {
        let def_fields = obj_fields.iter().map(|f| f.name.name.clone()).collect();
        let id = self.create_hirid_for(name.to_string(), HirType::Struct { fields: Vec::new() });
        self.objects_deffinitions.insert(id, def_fields);
        Ok(())
    }

    pub fn hoist_function(
        &mut self,
        name: &GenericIdentifier,
        args: &Vec<TypedName>,
        return_type: &GenericIdentifier,
    ) -> Result<()> {
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
    ) -> Result<SpecializedComponent> {
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
                        }
                        .into());
                    }
                },
                ComponentMemberValue::Child(e) => {
                    return Err(HIRError {
                        kind: HIRErrorKind::InvalidChild { child: Box::new(e) },
                        span: span.clone(),
                    }
                    .into());
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
                    prop_names: vec![String::from("text")],
                },
                span: span.clone(),
            }
            .into())
        }
    }

    pub fn resolve_component_defs(
        &mut self,
        def: Vec<ComponentMember>,
    ) -> Result<Vec<ComponentMemberDeclaration>> {
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
