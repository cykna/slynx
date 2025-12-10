use crate::{
    hir::types::HirType,
    intermediate::{IntermediateRepr, types::IntermediateType},
};

impl IntermediateRepr {
    pub fn retrieve_intermediate_type(&mut self, ty: &HirType) -> IntermediateType {
        match ty {
            HirType::Void => IntermediateType::Void,
            HirType::Float => IntermediateType::Float,
            HirType::Int => IntermediateType::Int,
            HirType::Str => IntermediateType::Str,
            HirType::Int8x4 => IntermediateType::Bytes,
            HirType::Int16x2 => IntermediateType::Words,
            HirType::Uint8x4 => IntermediateType::UBytes,
            HirType::Uint16x2 => IntermediateType::UWords,
            HirType::Component { .. } | HirType::GenericComponent => IntermediateType::Component,
            HirType::Reference { rf, .. } => IntermediateType::Reference(*rf),
            HirType::Function { args, return_type } => {
                let args = args
                    .iter()
                    .map(|arg| self.retrieve_intermediate_type(arg))
                    .collect();
                let ret = self.retrieve_intermediate_type(return_type);
                IntermediateType::Function(args, Box::new(ret))
            }
            un => unreachable!(
                "type '{un:?}' should be erased during type checking or not possible to be here"
            ),
        }
    }
}
