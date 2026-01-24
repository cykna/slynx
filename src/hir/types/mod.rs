mod tys;
pub use tys::*;

use std::collections::HashMap;
use crate::hir::{TypeId, symbols::SymbolPointer};

#[derive(Debug, Default)]
pub struct TypesModule {
    type_names:HashMap<SymbolPointer, TypeId>,
    types:HashMap<TypeId, HirType>,
}

macro_rules! hashmap {
    () => {
        HashMap::new()
    };
    ($($key:expr => $value:expr),*$(,)?) => ({
        let mut map =HashMap::new();
        $(map.insert($key, $value);)+
        map
    })
}
impl TypesModule {
    
    
    pub fn new() -> Self {
        Self {
            type_names: HashMap::new(),
            types: hashmap! {
                HirType::int_id() => HirType::Int,
                HirType::float_id() => HirType::Float,
                HirType::str_id() => HirType::Str,
                HirType::void_id() => HirType::Void,
                HirType::infer_id() => HirType::Infer,
            },
        }
    }
    
    pub fn insert_type(&mut self, name: SymbolPointer, ty: HirType) -> TypeId {
        let v = TypeId::new();
        self.type_names.insert(name, v);
        self.types.insert(v, ty);
        v
    }

    pub fn get_id(&self, name: &SymbolPointer) -> Option<&TypeId> {
        self.type_names.get(name)
    }
    pub fn get_type(&self, id: &TypeId) -> Option<&HirType> {
        self.types.get(id)
    }
    pub fn get_type_from_name(&self, name: &SymbolPointer) -> Option<&HirType> {
        self.type_names.get(name).and_then(|id| self.types.get(id))
    }
        
    pub fn get_type_mut(&mut self, id: &TypeId) -> Option<&mut HirType> {
        self.types.get_mut(id)
    }
    pub fn get_type_from_name_mut(&mut self, name: &SymbolPointer) -> Option<&mut HirType> {
        self.type_names.get(name).and_then(|id| self.types.get_mut(id))
    }
}
