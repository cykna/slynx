use std::borrow::Cow;

#[derive(Debug)]
pub enum IntermediateInstruction {
    ///Allocates(creates) a new function
    Alloc,
    ///Moves the expression in `value` into `target`
    Move {
        target: usize,
        value: usize,
    },
    ///Reads a variable with provided id
    Read(usize),
    ///Returns with the value on the provided id
    Ret(usize),
    Js(Cow<'static, str>),
}
