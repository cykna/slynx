mod tys;
pub use tys::*;

use std::collections::HashMap;
use crate::hir::{TypeId, symbols::SymbolPointer};

#[derive(Debug, Default)]
pub struct TypesModule {
    type_names:HashMap<SymbolPointer, TypeId>,
    types:HashMap<TypeId, HirType>,
}

impl TypesModule {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            type_names: HashMap::new(),
        }
    }

    pub fn retrieve_id(&self, name: &SymbolPointer) -> Option<&TypeId> {
        self.type_names.get(name)
    }
    pub fn retrieve_type(&self, id: &TypeId) -> Option<&HirType> {
        self.types.get(id)
    }
    pub fn retrieve_type_from_name(&self, name: &SymbolPointer) -> Option<&HirType> {
        self.type_names.get(name).and_then(|id| self.types.get(id))
    }
}
