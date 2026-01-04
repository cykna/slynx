use crate::hir::HirId;

#[derive(Debug)]
pub enum IntermediateInstruction {
    ///Allocates(creates) a new function
    Alloc(HirId),
    ///Moves the expression in `value` into `target`
    Move { target: usize, value: usize },
    ///Reads a variable with provided id
    Read(usize),
    ///Returns with the value on the provided id
    Ret(usize),
}
