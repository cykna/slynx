use crate::hir::{
    PropertyId, Result, SlynxHir,
    definitions::{ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind},
    error::{HIRError, HIRErrorKind},
    types::HirType,
};
use common::ast::{
    ComponentMember, ComponentMemberKind, GenericIdentifier, ObjectField, Span, TypedName,
};

impl SlynxHir {
    ///Retrieves the type of something by knowing the provided `ref_ty` is a reference to it
    pub fn get_type_from_ref(&self, ref_ty: crate::hir::TypeId) -> &HirType {
        if let HirType::Reference { rf, .. } = self.types_module.get_type(&ref_ty) {
            self.types_module.get_type(rf)
        } else {
            unreachable!("The provided ref_ty should be of type Reference");
        }
    }

    pub fn resolve_object(
        &mut self,
        name: GenericIdentifier,
        fields: Vec<ObjectField>,
        span: Span,
    ) -> Result<()> {
        let mut fields = {
            let mut out = Vec::with_capacity(fields.len());
            for field in &fields {
                let symbol_name = self.symbols_module.intern(&name.identifier);
                if self.symbols_module.intern(&field.name.name) == symbol_name {
                    return Err(HIRError::recursive(symbol_name, field.name.span).into());
                }
                out.push(
                    self.retrieve_information_of_type(
                        &field.name.kind.identifier,
                        &field.name.span,
                    )?
                    .0,
                );
            }
            out
        };
        let identifier_symbol = self.symbols_module.intern(&name.identifier);
        let (decl, declty) = if let Some(data) = self
            .declarations_module
            .retrieve_declaration_data_by_name(&identifier_symbol)
        {
            data
        } else {
            return Err(HIRError::name_unrecognized(identifier_symbol, name.span).into());
        };
        let HirType::Reference { rf, .. } = self.types_module.get_type(&declty) else {
            unreachable!("WTF, type of custom object should be a reference to its real type");
        };
        let rf = *rf;
        let HirType::Struct { fields: ty_field } = self.types_module.get_type_mut(&rf) else {
            unreachable!("WTF. Type of object should be a Struct ty");
        };

        ty_field.append(&mut fields);
        self.declarations.push(HirDeclaration {
            kind: HirDeclarationKind::Object,
            id: decl,
            ty: declty,
            span,
        });

        Ok(())
    }

    pub fn hoist_object(
        &mut self,
        name: &GenericIdentifier,
        obj_fields: &[ObjectField],
    ) -> Result<()> {
        let name = self.symbols_module.intern(&name.identifier);
        let def_fields = obj_fields
            .iter()
            .map(|f| self.symbols_module.intern(&f.name.name))
            .collect();
        let ty = self.types_module.insert_unnamed_type(HirType::Struct {
            fields: { Vec::new() },
        });

        let ty = self.define_type(
            name,
            HirType::Reference {
                rf: ty,
                generics: Vec::new(),
            },
        );

        self.declarations_module.create_object(name, ty, def_fields);
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
                let id = self.get_typeid_of_generic(&arg.kind)?;
                vec.push(id);
            }
            vec
        };
        let func_ty = HirType::Function {
            args,
            return_type: self.get_typeid_of_generic(return_type)?,
        };
        let symbol = self.symbols_module.intern(&name.identifier);
        let id = self.define_type(symbol, func_ty);
        self.create_declaration(&name.identifier, id);
        Ok(())
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
                        self.retrieve_information_of_type(&ty.identifier, &ty.span)?
                            .0
                    } else {
                        self.types_module.infer_id()
                    };

                    out.push(ComponentMemberDeclaration::Property {
                        id: PropertyId::new(),
                        index: prop_idx,
                        value: if let Some(rhs) = rhs {
                            Some(self.resolve_expr(rhs, Some(ty))?)
                        } else {
                            None
                        },
                        span: def.span,
                    });

                    self.create_variable(&name, ty, true);
                    prop_idx += 1;
                }
                ComponentMemberKind::Child(child) => {
                    let component = self.resolve_component(child)?;
                    out.push(component);
                }
            }
        }
        Ok(out)
    }
}
