use smallvec::SmallVec;

use crate::IRTypeId;

#[derive(Debug, Clone)]
pub struct Component {
    pub(crate) ty: IRTypeId,
    pub(crate) _children: SmallVec<[IRTypeId; 8]>,
}

impl Component {
    pub fn new(initial_type: IRTypeId) -> Self {
        Self {
            ty: initial_type,
            _children: SmallVec::new(),
        }
    }
}
