mod tys;
pub use tys::*;

use crate::hir::{TypeId, VariableId, symbols::SymbolPointer};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct TypesModule {
    ///A hashmap that maps a name of a global name to its type. This is not for variables, but only for global types, such as structs, functions and components
    type_names: HashMap<SymbolPointer, TypeId>,
    ///Maps a variable to it's type
    pub variables: HashMap<VariableId, TypeId>,

    types: Vec<HirType>,
}
impl TypesModule {
    pub fn new() -> Self {
        let types = vec![
            HirType::Int,
            HirType::Float,
            HirType::Str,
            HirType::Void,
            HirType::Infer,
            HirType::GenericComponent,
        ];
        //since type ids have a incremental index, it's a must to skip these, because HirType::int_id(), and the other ones are made to match this array
        for _ in &types {
            TypeId::new();
        }
        Self {
            type_names: HashMap::new(),
            variables: HashMap::new(),
            types,
        }
    }

    pub fn int_id(&self) -> TypeId {
        TypeId::from_raw(0)
    }
    pub fn float_id(&self) -> TypeId {
        TypeId::from_raw(1)
    }
    pub fn str_id(&self) -> TypeId {
        TypeId::from_raw(2)
    }
    pub fn void_id(&self) -> TypeId {
        TypeId::from_raw(3)
    }
    pub fn infer_id(&self) -> TypeId {
        TypeId::from_raw(4)
    }
    pub fn generic_component_id(&self) -> TypeId {
        TypeId::from_raw(5)
    }
    ///Inserts a new variable on this module
    pub fn insert_variable(&mut self, varid: VariableId, ty: TypeId) {
        self.variables.insert(varid, ty);
    }

    ///Inserts the provided `ty` to have the provided `name`
    pub fn insert_type(&mut self, name: SymbolPointer, ty: HirType) -> TypeId {
        let raw = self.types.len() as u64;
        let v = TypeId::from_raw(raw);
        self.type_names.insert(name, v);
        self.types.push(ty);
        v
    }

    ///Simply inserts the provided `ty` inside this module. Doesn't map it to anything
    pub fn insert_unnamed_type(&mut self, ty: HirType) -> TypeId {
        let id = TypeId::from_raw(self.types.len() as u64);
        self.types.push(ty);
        id
    }

    pub fn get_id(&self, name: &SymbolPointer) -> Option<&TypeId> {
        self.type_names.get(name)
    }
    pub fn get_type(&self, id: &TypeId) -> &HirType {
        &self.types[id.as_raw() as usize]
    }
    pub fn get_variable(&self, id: &VariableId) -> Option<&TypeId> {
        self.variables.get(id)
    }
    pub fn get_type_from_name(&self, name: &SymbolPointer) -> Option<&HirType> {
        self.type_names.get(name).map(|id| self.get_type(id))
    }

    pub fn get_type_mut(&mut self, id: &TypeId) -> &mut HirType {
        &mut self.types[id.as_raw() as usize]
    }
    pub fn get_type_from_name_mut(&mut self, name: &SymbolPointer) -> Option<&mut HirType> {
        self.type_names
            .get(name)
            .and_then(|id| Some(&mut self.types[id.as_raw() as usize]))
    }
}
