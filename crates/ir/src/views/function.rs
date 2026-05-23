use crate::{Function, IRFunction, IRStorage, IRType, IRTypeId, IRViewer, Label};

impl<'a> IRViewer<'a, Label> {}

impl<'a> IRViewer<'a, Function> {
    pub fn raw_type(&self) -> &IRFunction {
        let ty = self.ir.get(self.ptr).ty();
        let IRType::Function(id) = self.ir.types.get_type(ty) else {
            unreachable!();
        };
        self.ir.types.get_function_type(id)
    }
    pub fn get_arguments(&self) -> &[IRTypeId] {
        self.raw_type().get_args()
    }
    pub fn get_return_type(&self) -> IRTypeId {
        self.raw_type().get_return_type()
    }
    pub fn get_labels(&self) -> Vec<IRViewer<'a, Label>> {
        self.ir
            .get(self.ptr)
            .labels_ptr()
            .iter()
            .map(|ptr| IRViewer { ptr, ir: self.ir })
            .collect()
    }
}
