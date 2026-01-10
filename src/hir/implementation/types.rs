use color_eyre::eyre::Result;

use crate::{
    hir::{
        HirId, SlynxHir,
        error::{HIRError, HIRErrorKind},
        types::HirType,
    },
    parser::ast::{GenericIdentifier, Span},
};

impl SlynxHir {
    ///Creates an hir id for the provided `value` and `name` on the current scope
    pub fn create_hirid_for(&mut self, name: String, ty: HirType) -> HirId {
        let id = HirId::new();
        self.names.insert(name.clone(), id);
        self.last_scope().insert_name(id, name);
        self.types.insert(id, ty);
        id
    }

    ///Retrieves the type of the provided `name` but in the global scope
    pub fn retrieve_type_of_name(
        &mut self,
        name: &GenericIdentifier,
        span: &Span,
    ) -> Result<HirType> {
        match HirType::new(name) {
            Ok(value) => Ok(value),
            Err(_) => {
                if let Some(name_id) = self.names.get(&name.identifier)
                    && let Some(_) = self.types.get(name_id)
                {
                    Ok(HirType::Reference {
                        rf: *name_id,
                        generics: Vec::new(),
                    })
                } else {
                    Err(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                        span: span.clone(),
                    }
                    .into())
                }
            }
        }
    }
    ///Tries to retrieve the type and HirId of the provided `name` in the global scope
    pub fn retrieve_information_of(&mut self, name: &str, span: &Span) -> Result<(HirId, HirType)> {
        if let Some(name_id) = self.names.get(name)
            && let Some(ty) = self.types.get(name_id)
        {
            Ok((*name_id, ty.clone()))
        } else {
            Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            }
            .into())
        }
    }
    ///Retrieves the type of the provided `name` but in the global scope. The difference of a 'named' to a 'name' is that this function
    ///tries to the the provided `name` as some identifier to something, and the name version does so after checking if the provided name itself
    ///is a type
    pub fn retrieve_type_of_named(&mut self, name: &str, span: &Span) -> Result<HirType, HIRError> {
        if let Some(name_id) = self.names.get(name)
            && let Some(ty) = self.types.get(name_id)
        {
            Ok(ty.clone())
        } else {
            Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            })
        }
    }

    pub fn retrieve_ref_to_type(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<&mut HirType, HIRError> {
        if let Some(name_id) = self.names.get(name)
            && let Some(ty) = self.types.get_mut(name_id)
        {
            Ok(ty)
        } else {
            Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            })
        }
    }
}
