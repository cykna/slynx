use crate::IRTypeId;

#[derive(Debug, Clone)]
pub struct Component {
    pub(crate) ty: IRTypeId,
}

impl Component {
    pub fn new(initial_type: IRTypeId) -> Self {
        Self { ty: initial_type }
    }
}
