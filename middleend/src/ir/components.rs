use frontend::hir::{
    TypeId,
    definitions::ComponentMemberDeclaration,
    types::{HirType, TypesModule},
};

use crate::{Component, IRError, IRPointer, IRType, SlynxIR, ir::temp::TempIRData};

impl SlynxIR {
    /// Inserts the contents of the provided `decl` into an IR struct type asserting it's a slynx component. This is made because component can be lowered to the equivalent of a struct with methods, thus 'classes'.
    /// The thing is that this is made interanlly with the minimum of abstraction as possible, so it becomes a struct and the components as well as methods are inserted directly into the struct as fields
    pub(crate) fn insert_component_fields_for(
        &mut self,
        decl: TypeId,
        temp: &mut TempIRData,
        tys: &TypesModule,
    ) -> Result<(), IRError> {
        let component_type = self.get_ir_type(&decl, temp, tys)?;
        let IRType::Component(cid) = self.types.get_type(component_type) else {
            unreachable!("Something errored that type of component simply isnt Component on ir");
        };

        let Some(HirType::Component { props: ty_props }) = tys.get_component(&decl) else {
            unreachable!("{:?} should map to an Component, but it doesn't", decl);
        };

        for (_, _, prop) in ty_props {
            let ty = self.get_ir_type(prop, temp, tys)?;
            let comp_ty = self.types.get_component_type_mut(cid);
            comp_ty.insert_field(ty);
        }
        Ok(())
    }

    pub fn get_component_expression(
        &mut self,
        name: TypeId,
        values: &[ComponentMemberDeclaration],
    ) {
        let value = self.get_empty_component_expr(name);
    }

    pub fn initialize_component(
        &mut self,
        _: IRPointer<Component, 1>,
        props: &[ComponentMemberDeclaration],
        _temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        //let component = self.get_component_mut(comp);
        for prop in props {
            match prop {
                ComponentMemberDeclaration::Property { .. } => {
                    //already implemented on insert_component_fields
                }
                ComponentMemberDeclaration::Child { name, values, span } => {
                    let component = self.get_component_mut(comp.clone());
                    let child = self.get_component_expression(*name, values);
                }
                ComponentMemberDeclaration::Specialized(_) => {}
            }
        }
        Ok(())
    }
}
