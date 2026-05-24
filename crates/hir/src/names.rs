use crate::{
    Result, SlynxHir, TypeId, VariableId,
    error::{HIRError, InvalidTypeReason},
    model::HirType,
};

use common::{Span, SymbolPointer};
//file specific to implement things related to name resolution
impl SlynxHir {
    pub fn intern_name(&mut self, name: &str) -> SymbolPointer {
        self.modules.intern_name(name)
    }
    ///Since when a object is defined, its generated as an unnamed type, and has got a reference to it, this retrieves the inner layout of the object
    pub(crate) fn get_object_type_from_name(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<&HirType> {
        let name_symbol = self.modules.intern_name(name);
        if let Some(ref_id) = self.modules.types_module.get_id(&name_symbol)
            && let HirType::Reference { rf, .. } = self.modules.types_module.get_type(ref_id)
        {
            Ok(self.modules.types_module.get_type(rf))
        } else {
            Err(HIRError::invalid_type(
                name_symbol,
                InvalidTypeReason::IncorrectUsage,
                *span,
            ))
        }
    }

    ///Creates a mutable variable with the given `name` and `ty`
    pub(crate) fn create_mutable_variable(
        &mut self,
        symbol: SymbolPointer,
        ty: TypeId,
        span: &Span,
    ) -> Result<VariableId> {
        self.modules.create_variable(symbol, true, ty, span)
    }
    ///Creates a imutable variable with the given `name` and `ty`
    pub(crate) fn create_variable(
        &mut self,
        symbol: SymbolPointer,
        ty: TypeId,
        span: &Span,
    ) -> Result<VariableId> {
        self.modules.create_variable(symbol, false, ty, span)
    }

    ///Tries to retrieve the type and `TypeId` of the provided `name` in the global scope
    pub(crate) fn retrieve_information_of_type(
        &mut self,
        name: &str,
        span: &Span,
    ) -> Result<(TypeId, &HirType)> {
        let name_symbol = self.modules.intern_name(name);
        match () {
            _ if let Ok(id) = self.get_type_of_name(name_symbol, span) => {
                Ok((id, self.get_type(&id)))
            }
            _ => Err(HIRError::name_unrecognized(name_symbol, *span)),
        }
    }
}
