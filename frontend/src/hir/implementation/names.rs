use crate::hir::{DeclarationId, Result, SlynxHir, TypeId, error::HIRError, types::HirType};

use common::ast::{GenericIdentifier, Span};
impl SlynxHir {
    pub fn create_declaration(&mut self, name: &str, ty: TypeId) -> DeclarationId {
        let ptr = self.symbols_module.intern(name);
        self.declarations_module.create_declaration(ptr, ty)
    }

    ///Creates a type with the provided `name` and `ty`. Returns it's ID
    pub fn assing_type(&mut self, type_id: TypeId, ty: HirType) {
        self.types.insert(type_id, ty);
    }

    ///Retrieves the type of the provided `name` but in the global scope
    pub fn retrieve_type_of_name(
        &mut self,
        name: &GenericIdentifier,
        span: &Span,
    ) -> Result<HirType> {
        match HirType::new(&name.identifier) {
            Some(value) => Ok(value),
            _ => {
                let name_id = self.symbols_module.intern(&name.identifier);
                if let Some(ty) = self.types_module.get_id(&name_id) {
                    Ok(HirType::Reference {
                        rf: *ty,
                        generics: Vec::new(),
                    })
                } else {
                    Err(HIRError::name_unrecognized(name_id, *span))
                }
            }
        }
    }
    ///Tries to retrieve the type and `TypeId` of the provided `name` in the global scope
    pub fn retrieve_information_of_type(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<(TypeId, &HirType)> {
        let name_symbol = self.symbols_module.intern(name);
        match () {
            _ if let Ok(id) = self.get_typeid_of_name(name, span) => {
                Ok((id, self.types_module.get_type(&id)))
            }
            _ if let Some(id) = self.types_module.get_id(&name_symbol) => {
                Ok((*id, self.types_module.get_type(id)))
            }
            _ => Err(HIRError::name_unrecognized(name_symbol, *span)),
        }
    }
    ///Retrieves the type of the provided `name` but in the global scope. The difference of a 'named' to a 'name' is that this function
    ///tries to the the provided `name` as some identifier to something, and the name version does so after checking if the provided name itself
    ///is a type
    pub fn retrieve_type_of_named(&mut self, name: &str, span: &Span) -> Result<&HirType> {
        let name_id = self.symbols_module.intern(name);
        match self.types_module.get_type_from_name(&name_id) {
            Some(ty) => Ok(ty),
            _ => Err(HIRError::name_unrecognized(name_id, *span)),
        }
    }

    pub fn retrieve_ref_to_type(&mut self, name: &str, span: &Span) -> Result<&mut HirType> {
        let name_symbol = self.symbols_module.intern(name);
        match self.types_module.get_type_from_name_mut(&name_symbol) {
            Some(ty) => Ok(ty),
            _ => Err(HIRError::name_unrecognized(name_symbol, *span)),
        }
    }
}
