use common::SymbolPointer;

use crate::{IRStructId, IRType, IRTypeId, SlynxIR};

pub struct StructBuilder<'a> {
    ir: &'a mut SlynxIR,
    ir_struct_id: IRStructId,
}

impl<'a> StructBuilder<'a> {
    ///Creates a new Struct builder with the given `ty` and inner `ir` to build properly. Returns error if the given `ty` is not a struct type
    pub fn new(name: SymbolPointer, ir: &'a mut SlynxIR) -> Self {
        let ty = ir.types.create_empty_struct(name);
        let IRType::Struct(sid) = ir.types.get_type(ty) else {
            unreachable!();
        };
        Self {
            ir,
            ir_struct_id: sid,
        }
    }
    pub fn insert_type(&mut self, ty: IRTypeId) {
        self.ir
            .types
            .get_object_type_mut(self.ir_struct_id)
            .insert_field(ty);
    }

    pub fn get_field(&self, index: usize) -> IRTypeId {
        self.ir
            .types
            .get_object_type(self.ir_struct_id)
            .get_fields()[index]
    }

    pub fn generate(self) {}
}
