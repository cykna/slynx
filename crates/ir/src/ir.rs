use crate::views::IRStorage;
use common::{SymbolPointer, SymbolsModule};

use crate::{
    Component, Formatter, Function, IRPointer, IRTypes, Instruction, Label, Operand, Slot, Value,
};

#[derive(Debug)]
///All the IR containing contexts, labels, instructions and operands
pub struct SlynxIR {
    ///The contexts of this IR
    pub(crate) functions: Vec<Function>,
    ///The Components of this IR
    pub(crate) components: Vec<Component>,
    ///The labels of this IR
    pub(crate) labels: Vec<Label>,
    ///The instructions of this IR
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) instruction_pointers: Vec<IRPointer<Instruction>>,
    ///The operands of this IR
    pub(crate) operands: Vec<Operand>,
    ///The values of this IR
    pub(crate) values: Vec<Value>,
    pub(crate) slots: Vec<Slot>,
    pub(crate) types: IRTypes,
    ///Pool of interned strings, accessed via StringHandle indices
    pub(crate) strings: SymbolsModule,
}

impl SlynxIR {
    ///Creates a new empty IR
    pub fn new(symbols: SymbolsModule) -> Self {
        Self {
            components: Vec::new(),
            functions: Vec::new(),
            labels: Vec::new(),
            instructions: Vec::new(),
            instruction_pointers: Vec::new(),
            operands: Vec::new(),
            values: Vec::new(),
            slots: Vec::new(),
            types: IRTypes::new(),
            strings: symbols,
        }
    }

    ///Returns a reference to the string pool
    pub fn string_pool(&self) -> &SymbolsModule {
        &self.strings
    }

    /// Produces a Slynx IR textual dump (SIR) following the README syntax.
    ///
    /// This uses the helpers defined in the `visualize` module to format labels and
    /// instructions in the human-readable SIR form described in `middleend/README.md`.
    pub fn format_sir(&self) -> String {
        let fmt = Formatter::new(self, &self.strings);
        let mut out = fmt.format_types();
        out.push_str(&&fmt.format_functions());
        out
    }

    ///Retrieves the next label pointer
    pub fn get_next_label_ptr(&self) -> IRPointer<Label, 1> {
        IRPointer::new(self.labels.len(), 1)
    }
    ///Retrieves the next label pointer
    pub fn get_next_mapeable_instruction_ptr(&self) -> IRPointer<IRPointer<Instruction>, 1> {
        IRPointer::new(self.instruction_pointers.len(), 1)
    }

    ///Creates a new label and returns its pointer.
    fn create_label(&mut self, name: SymbolPointer) -> IRPointer<Label, 1> {
        let ptr = self.labels.len();
        self.labels.push(Label::new(name));
        IRPointer::new(ptr, 1)
    }

    ///Inserts a new label into the given context and returns its pointer. Determines for the label to have the provided `label` name.
    pub(crate) fn insert_label(
        &mut self,
        ir: IRPointer<Function, 1>,
        label: &str,
    ) -> IRPointer<Label, 1> {
        self.functions[ir.ptr()].insert_label(); //this just increases the label count on the context
        let name = self.strings.intern(label);
        let label_ptr = self.create_label(name);
        // Initialize the instruction pointer offset to the current end of instruction_pointers,
        // so instructions inserted later are correctly attributed to this label.
        let next = self.get_next_mapeable_instruction_ptr();
        let mut ptr = next.with_length();
        ptr.set_length(ptr.len() - 1);
        self.get_mut(label_ptr).set_instructions_pointer(ptr);
        label_ptr
    }

    pub fn insert_values(&mut self, value: &[Value]) -> IRPointer<Value> {
        let ptr = self.values.len();
        self.values.extend_from_slice(value);
        IRPointer::new(ptr, value.len())
    }

    ///Inserts the given value and returns its pointer
    pub fn insert_value(&mut self, value: Value) -> IRPointer<Value, 1> {
        let ptr = self.values.len();
        self.values.push(value);
        IRPointer::new(ptr, 1)
    }
}
