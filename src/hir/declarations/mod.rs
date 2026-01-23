use crate::hir::{DeclarationId, symbols::SymbolPointer, types::HirType};
use std::collections::HashMap;

/// A top level module that keeps track of all the declarations on the Hir.
/// Since declarations are avaible only on the top level this is being implemented by thinking in so
#[derive(Debug, Default)]
pub struct DeclarationsModule {
    decls: HashMap<SymbolPointer, DeclarationId>,
    ///The types of the declarations. Use a vec because we can access the type based on the inner value of the ID
    declaration_types: Vec<HirType>,
    objects: HashMap<DeclarationId, Vec<SymbolPointer>>,
}

impl DeclarationsModule {
    pub fn new() -> Self {
        DeclarationsModule {
            decls: HashMap::new(),
            objects: HashMap::new(),
            declaration_types: Vec::new(),
        }
    }
    pub fn create_declaration(&mut self, name: SymbolPointer, ty: HirType) -> DeclarationId {
        let id = DeclarationId::new();
        self.decls.insert(name, id);
        self.declaration_types.push(ty);
        id
    }
    ///Creates an objct with the provided `name`, `ty` and `fields` and returns it's id
    pub fn create_object(
        &mut self,
        name: SymbolPointer,
        ty: HirType,
        fields: Vec<SymbolPointer>,
    ) -> DeclarationId {
        let id = DeclarationId::new();
        self.decls.insert(name, id);
        self.declaration_types.push(ty);
        self.objects.insert(id, fields);
        id
    }
}
