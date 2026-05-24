use common::{Span, SymbolPointer};

use crate::{HIRError, HirType, Result, SlynxHir, VariableId};

impl SlynxHir {
    /// Returns the source-level name string for the given symbol pointer.
    pub fn get_name_from_pointer(&self, ptr: SymbolPointer) -> &str {
        self.modules.symbols_resolver.get_name(ptr)
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
