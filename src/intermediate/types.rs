use crate::hir::HirId;

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
    Complex(Vec<IntermediateType>),
    Vector(Box<IntermediateType>), //vec of a type
    Reference(HirId),
    ///A function. The first value is the args, and the second one is the return type
    Function(Vec<IntermediateType>, Box<IntermediateType>),
}
