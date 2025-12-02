#[derive(Debug, Clone)]
pub enum IntermediateType {
    Void,
    Int,
    Float,
    Words,
    UWords,
    Bytes,
    UBytes,
    Component,
    Str,
    ///Index inside the intermediate repr
    Complex(Vec<usize>),
    Vector(usize), //vec of a type
    Reference(usize),
    ///A function. The first value is the args, and the second one is the return type
    Function(Vec<usize>, usize),
}
