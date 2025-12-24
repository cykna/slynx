use crate::{hir::{HirId, types::HirType}, intermediate::IntermediateRepr};

#[derive(Debug, Clone)]
pub enum IntermediateType {
    Void,
    Int,
    Float,
    Component,
    Str,
    ///Index inside the intermediate repr
    Complex(Vec<IntermediateType>),
    Vector(Box<IntermediateType>), //vec of a type
    Reference(HirId),
    ///A function. The first value is the args, and the second one is the return type
    Function(Vec<IntermediateType>, Box<IntermediateType>),
}

impl IntermediateRepr {
    pub fn get_type(&self, ty:&HirType) -> IntermediateType {
        match ty {
            
            HirType::Int => IntermediateType::Int,
            HirType::Float => IntermediateType::Float,
            HirType::Str => IntermediateType::Str,
            HirType::Void => IntermediateType::Void,
            HirType::Vector { ty } => IntermediateType::Vector(Box::new(self.get_type(&*ty))),
            HirType::GenericComponent => IntermediateType::Component,
            HirType::Reference { rf, .. } => {
               IntermediateType::Reference(*rf) 
            }
            HirType::Function { .. } | HirType::Struct { ..} | HirType::Component { .. } => unreachable!("All struct types should be reference instead"),
            un => unimplemented!("{un:?}")
        }
    }
}