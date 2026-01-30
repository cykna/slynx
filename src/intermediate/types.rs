use crate::{
    hir::{
        TypeId,
        types::{HirType, TypesModule},
    },
    intermediate::IntermediateRepr,
};

#[derive(Debug, Clone)]
pub enum IntermediateType {
    Void,
    Int,
    Float,
    Bool,
    Component,
    Str,
    ///Index inside the intermediate repr
    Complex(Vec<IntermediateType>),
    Vector(Box<IntermediateType>), //vec of a type
    Reference(TypeId),
    ///A function. The first value is the args, and the second one is the return type
    Function(Vec<IntermediateType>, Box<IntermediateType>),
}

impl IntermediateRepr {
    ////Creates a new complex type from the provided `tys` and returns it
    pub fn retrieve_complex(&mut self, tys: &[TypeId], module: &TypesModule) -> IntermediateType {
        IntermediateType::Complex(tys.iter().map(|t| self.get_type(t, module)).collect())
    }

    pub fn get_type(&self, ty: &TypeId, module: &TypesModule) -> IntermediateType {
        match module.get_type(ty) {
            HirType::Int => IntermediateType::Int,
            HirType::Float => IntermediateType::Float,
            HirType::Bool => IntermediateType::Bool,
            HirType::Str => IntermediateType::Str,
            HirType::Void => IntermediateType::Void,
            HirType::Vector { ty } => {
                let vecty = self.get_type(ty, module);
                IntermediateType::Vector(Box::new(vecty))
            }
            HirType::GenericComponent => IntermediateType::Component,
            HirType::Reference { rf, .. } => IntermediateType::Reference(*rf),
            HirType::Function { .. } | HirType::Struct { .. } | HirType::Component { .. } => {
                unreachable!("All struct types should be reference instead")
            }
            un => unimplemented!("{un:?}"),
        }
    }
}
