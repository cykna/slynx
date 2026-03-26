use frontend::hir::definitions::ComponentMemberDeclaration;

use crate::{Component, IRError, IRPointer, SlynxIR, ir::temp::TempIRData};

impl SlynxIR {
    pub fn initialize_component(
        &mut self,
        comp: IRPointer<Component, 1>,
        _props: &[ComponentMemberDeclaration],
        _temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        let _component = self.get_component(comp);

        Ok(())
    }
}
