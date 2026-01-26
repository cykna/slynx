use crate::{
    hir::VariableId,
    intermediate::id::{GlobalId, ValueId},
};

#[derive(Debug)]
pub enum IntermediatePlace {
    ///A reference to a variable
    Local(VariableId),
    Field {
        parent: VariableId,
        field: usize,
    },
}

#[derive(Debug)]
pub struct IntermediateInstruction {
    pub id: GlobalId,
    pub kind: IntermediateInstructionKind,
}

#[derive(Debug)]
pub enum IntermediateInstructionKind {
    ///Allocates(creates) a new function
    Alloc(VariableId),
    ///Moves the expression in `value` into `target`
    Move {
        target: IntermediatePlace,
        value: ValueId,
    },
    ///Reads a variable with provided id
    Read(VariableId),
    ///Returns with the value on the provided id
    Ret(ValueId),
}

impl IntermediateInstruction {
    pub fn alloc(varialbe_id: VariableId) -> Self {
        Self {
            id: GlobalId::new(),
            kind: IntermediateInstructionKind::Alloc(varialbe_id),
        }
    }
    pub fn mov(target: IntermediatePlace, value: ValueId) -> Self {
        Self {
            id: GlobalId::new(),
            kind: IntermediateInstructionKind::Move { target, value },
        }
    }
    pub fn read(varialbe_id: VariableId) -> Self {
        Self {
            id: GlobalId::new(),
            kind: IntermediateInstructionKind::Read(varialbe_id),
        }
    }
    pub fn ret(varialbe_id: ValueId) -> Self {
        Self {
            id: GlobalId::new(),
            kind: IntermediateInstructionKind::Ret(varialbe_id),
        }
    }
}
