use either::Either::{self, Left, Right};

use crate::{
    Component, ControlFlowGraph, Function, IRPointer, IRTypeId, IRTypes, IRViewer, Instruction,
    Label, Operand, SlynxIR, Value,
    builder::{FunctionBuilder, StructBuilder},
};
pub type InstructionPtr<const K: usize = 0> =
    Either<IRPointer<IRPointer<Instruction, K>, 1>, IRPointer<Instruction, K>>;
impl SlynxIR {
    ///Creates a new empty struct with the given name
    pub fn create_struct(&mut self, name: &str) -> IRTypeId {
        let name = self.strings.intern(name);
        self.types.create_empty_struct(name).0
    }

    pub fn build_struct<'a>(&'a mut self, ty: IRTypeId) -> Result<StructBuilder<'a>, ()> {
        StructBuilder::new(ty, self)
    }
    ///Creates a new empty function with the given name and returns it's pointer on the IR
    pub fn create_function(&mut self, name: &str) -> IRPointer<Function, 1> {
        let name = self.strings.intern(name);
        let func = Function::new(name, self.types.create_empty_function().0);
        let ptr = self.functions.len(); //since we push, len = next index
        self.functions.push(func);
        IRPointer::new(ptr, 1)
    }
    pub fn build_function<'a>(&'a mut self, ptr: IRPointer<Function, 1>) -> FunctionBuilder<'a> {
        FunctionBuilder::new(ptr, self)
    }

    pub fn generate_function_cfg(&self, function: &Function) -> ControlFlowGraph {
        let labels = function.labels_ptr().with_length();
        ControlFlowGraph::new(labels, self)
    }

    pub fn functions(&self) -> &[Function] {
        &self.functions
    }

    pub fn components(&self) -> &[Component] {
        &self.components
    }

    pub fn get_function_labels(&self, ctx: &Function) -> &[Label] {
        let ptr = ctx.labels_ptr();
        &self.labels[ptr.range()]
    }

    pub fn get_label_instructions(&self, label: &Label) -> Vec<&[Instruction]> {
        let ptr = label.instructions();
        self.get_multiple_instructions(ptr)
    }

    pub fn get_label_instructions_with_ptrs(
        &self,
        label: &Label,
    ) -> Vec<(&[Instruction], IRPointer<Instruction>)> {
        let ptr = label.instructions();
        self.get_multiple_instructions_and_ptrs(ptr)
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
    pub fn get_values_by_pointer(&self, ptr: IRPointer<Value>) -> &[Value] {
        &self.values[ptr.range()]
    }

    ///Retrieves all the instructions that are pointer by the given `ptr`, since its the same as **instruction, this returns a vector containing the instructions returned by each
    ///pointer**
    pub fn get_multiple_instructions(
        &self,
        ptr: IRPointer<IRPointer<Instruction>>,
    ) -> Vec<&[Instruction]> {
        let ptrs = &self.instruction_pointers[ptr.range()];
        let mut out = Vec::with_capacity(ptrs.len());
        for ptr in ptrs {
            out.push(&self.instructions[ptr.range()]);
        }
        out
    }
    ///Retrieves all the instructions that are pointer by the given `ptr`, since its the same as **instruction, this returns a vector containing the instructions returned by each
    ///pointer
    pub fn get_multiple_instructions_and_ptrs(
        &self,
        ptr: IRPointer<IRPointer<Instruction>>,
    ) -> Vec<(&[Instruction], IRPointer<Instruction>)> {
        let ptrs = &self.instruction_pointers[ptr.range()];
        let mut out = Vec::with_capacity(ptrs.len());
        for ptr in ptrs {
            out.push((&self.instructions[ptr.range()], *ptr));
        }
        out
    }
    ///Retriueves the instructions pointed by the given `ptr`
    pub fn get_instruction_by_pointer(&self, ptr: IRPointer<Instruction>) -> &[Instruction] {
        &self.instructions[ptr.range()]
    }

    pub fn get_view<'a, T>(&'a self, ptr: IRPointer<T, 1>) -> IRViewer<'a, T> {
        IRViewer { ptr, ir: self }
    }

    #[inline]
    pub fn dereference_instruction_ptr(&self, ptr: InstructionPtr) -> IRPointer<Instruction> {
        match ptr {
            Left(ptr) => self.instruction_pointers[ptr.ptr()].with_length(),
            Right(e) => e.with_length(),
        }
    }
}
