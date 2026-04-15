use crate::{Context, IRPointer, IRTypes, Instruction, Label, Operand, SlynxIR, Value};

impl SlynxIR {
    pub fn contexts(&self) -> &[Context] {
        &self.contexts
    }

    pub fn get_context_labels(&self, ctx: &Context) -> &[Label] {
        let ptr = ctx.labels_ptr();
        &self.labels[ptr.range()]
    }

    pub fn get_label_instructions(&self, label: &Label) -> &[Instruction] {
        let ptr = label.instruction();
        &self.instructions[ptr.range()]
    }

    ///Retrieves the inner struct that manages the types on the IR
    pub fn ir_types(&self) -> &IRTypes {
        &self.types
    }

    ///Retrieves a `operand` array that is pointed by the given ptr
    pub fn get_operand_by_pointer(&self, ptr: IRPointer<Operand>) -> &[Operand] {
        &self.operands[ptr.range()]
    }
    ///Retrieves a `value` array that is pointed by the given ptr
    pub fn get_values_by_pointer(&self, ptr: IRPointer<Instruction>) -> &[Value] {
        &self.values[ptr.range()]
    }

    ///Retrieves a `instruction` array that is pointed by the given ptr
    pub fn get_instructions_by_pointer(&self, ptr: IRPointer<Instruction>) -> &[Instruction] {
        &self.instructions[ptr.range()]
    }
}
