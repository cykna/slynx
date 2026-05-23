use crate::{Component, IRPointer, IRTypeId, Operand, SlynxIR, TempIRData, Value};

impl SlynxIR {
    pub(crate) fn create_void_value(&self) -> Value {
        Value::new_void(self.types.void_type())
    }

    pub(crate) fn create_func_arg_value(&self, index: usize, temp: &TempIRData) -> Value {
        let arg_types = self.arg_types_of_context(temp.current_function());
        Value::new_func_arg(index, arg_types[index])
    }

    pub(crate) fn create_raw_value(&self, operand: IRPointer<Operand, 1>) -> Value {
        let ty = self.get_operand_type(operand);
        Value::new_raw(operand, ty)
    }
    pub(crate) fn create_component_child_value(
        &self,
        index: usize,
        comp: IRPointer<Component, 1>,
    ) -> Value {
        let component = self.get_component(comp);
        let crate::IRType::Component(ty) = self.types.get_type(component.ir_type()) else {
            unreachable!();
        };
        let child_ty = self.types.get_component_type(ty).children()[index];
        Value::new_component_child(index, child_ty)
    }

    pub(crate) fn get_type_of_value(&self, ptr: IRPointer<Value, 1>) -> IRTypeId {
        self.values[ptr.ptr()].ir_type()
    }
}
