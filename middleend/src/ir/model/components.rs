use crate::{IRPointer, IRTypeId, Value};

#[derive(Debug, Clone)]
pub struct DefaultArgument {
    ptr: IRPointer<Value>,
    field_index: usize,
}

#[derive(Debug, Clone)]
pub struct Component {
    defaults: DefaultArgument,
    pub(crate) ty: IRTypeId,
}

impl Component {
    pub fn new(initial_type: IRTypeId) -> Self {
        Self {
            defaults: DefaultArgument {
                ptr: IRPointer::null(),
                field_index: 0,
            },
            ty: initial_type,
        }
    }
}
