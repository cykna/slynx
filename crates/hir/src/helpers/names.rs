use common::{Span, SymbolPointer};
use slynx_parser::GenericIdentifier;

use crate::{HIRError, HirType, Result, SlynxHir, VariableId};

impl SlynxHir {
    /// Returns the source-level name string for the given symbol pointer.
    pub fn retrieve_name_from_pointer(&self, ptr: SymbolPointer) -> &str {
        self.modules.symbols_resolver.get_name(ptr)
    }
    ///Retrieves the type of the provided `name` but in the global scope. The difference of a 'named' to a 'name' is that this function
    ///tries to the the provided `name` as some identifier to something, and the name version does so after checking if the provided name itself
    ///is a type
    pub fn get_type_from_name(&mut self, name: &str, span: &Span) -> Result<&HirType> {
        let name_id = self.modules.intern_name(name);
        match self.get_type_from_pointer(&name_id) {
            Some(ty) => Ok(ty),
            _ => Err(HIRError::name_unrecognized(name_id, *span)),
        }
    }
    /// Retrieves a mutable reference to the [`HirType`] registered under the given name.
    ///
    /// Returns an error if no type with that name exists.
    pub fn get_reference_to_type(&mut self, name: &str, span: &Span) -> Result<&mut HirType> {
        let name_symbol = self.modules.intern_name(name);
        match self.get_type_mut_from_name(&name_symbol) {
            Some(ty) => Ok(ty),
            _ => Err(HIRError::name_unrecognized(name_symbol, *span)),
        }
    }
    ///Retrieves the type of the provided `name` but in the global scope
    pub fn get_type_of_name(&mut self, name: &GenericIdentifier, span: &Span) -> Result<HirType> {
        let name_id = self.modules.intern_name(&name.identifier);
        match HirType::new(&name.identifier) {
            Some(value) => Ok(value),
            _ if let Some(ty) = self.get_typeid_from_name(&name_id) => Ok(HirType::new_ref(*ty)),
            _ => Err(HIRError::name_unrecognized(name_id, *span)),
        }
    }
    ///Tries to retrieve a variable with the provided `name` on the current active scope
    pub fn get_variable(&mut self, symbol: SymbolPointer, span: &Span) -> Result<VariableId> {
        if let Some(variable) = self.modules.find_variable(symbol) {
            Ok(variable)
        } else {
            Err(HIRError::name_unrecognized(symbol, *span))
        }
    }
    ///Retrieves the pointer(simply a symbol) of the provided `name`.
    pub fn get_symbol(&self, name: &str) -> Option<SymbolPointer> {
        self.modules.retrieve_symbol(name)
    }
}
