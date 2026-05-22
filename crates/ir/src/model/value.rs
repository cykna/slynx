use std::ops::Deref;

use common::SymbolPointer;

use crate::{IRPointer, IRTypeId, Instruction};

#[derive(Debug, Clone, Copy)]
///A value that represents something on a slot. A slot is something on memory, anywhere, so this is practically a pointer to some value. But it's better to be
///understood as a variable
pub struct Slot {
    pub(crate) ty: IRTypeId,
}

#[derive(Debug, Clone)]
///An operand that is used on a instruction. Mainly it's values that are used on the instructions
pub enum Operand {
    Bool(bool),
    Int(i64),
    Float(f64),
    ///Index into the SlynxIR string pool's entries. The backend resolves this to a StrHandle {offset, len}.
    String(SymbolPointer),
}

#[derive(Debug, Clone)]
pub struct Value {
    ///The kind of this value
    kind: ValueKind,
    ///The type of this value
    ty: IRTypeId,
}

#[derive(Debug, Clone)]
///A value inside the IR. Can be a function arg, a label arg or the result of a instruction
pub enum ValueKind {
    Void,
    StructLiteral(IRPointer<Value>),
    Raw(IRPointer<Operand, 1>),
    Instruction(IRPointer<Instruction, 1>),
    Slot(IRPointer<Slot, 1>),
    LabelArg(usize),
    FuncArg(usize),
    ComponentChild(usize),
    /// A symbolic reference to a property of the enclosing component instance.
    ///
    /// `ComponentProperty(index)` is resolved at runtime to the actual value of
    /// the component's property at the given field index. This is analogous to
    /// [`ComponentChild`], but for properties instead of children.
    ///
    /// This value kind exists because component properties are reactive fields,
    /// not plain variables — their runtime values are set by the parent and read
    /// during component initialization. The IR cannot evaluate them to a constant,
    /// so it emits a symbolic handle that the runtime resolves.
    ComponentProperty(usize),
}

impl Value {
    ///Creates a new struct with the given `ty` and `values`. It should be asserted to be a struct on the IR when generating
    pub fn new_struct(ty: IRTypeId, values: IRPointer<Value>) -> Self {
        Self {
            kind: ValueKind::StructLiteral(values),
            ty,
        }
    }
    ///Creates a new value that maps to the `arg_index` argument of the label it is being inserted at
    pub fn new_label_arg(ty: IRTypeId, arg_index: usize) -> Self {
        Self {
            kind: ValueKind::LabelArg(arg_index),
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_instruction(ptr: IRPointer<Instruction, 1>, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::Instruction(ptr),
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_func_arg(index: usize, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::FuncArg(index),
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_void(ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::Void,
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_raw(operand: IRPointer<Operand, 1>, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::Raw(operand),
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_slot(ptr: IRPointer<Slot, 1>, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::Slot(ptr),
            ty,
        }
    }
    //Creates a new value that maps to a instruction
    pub fn new_component_child(index: usize, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::ComponentChild(index),
            ty,
        }
    }
    ///Creates a new symbolic reference to a property of the enclosing component
    pub fn new_component_property(index: usize, ty: IRTypeId) -> Self {
        Self {
            kind: ValueKind::ComponentProperty(index),
            ty,
        }
    }
    ///Retrieves the IR type of this Value
    pub fn ir_type(&self) -> IRTypeId {
        self.ty
    }
}

impl Deref for Value {
    type Target = ValueKind;
    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}
