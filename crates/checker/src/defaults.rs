//* File specific for defining default types on declarations
use slynx_hir::model::{HirDeclaration, HirDeclarationKind, HirType};

use crate::{Result, TypeChecker};

impl TypeChecker {
    pub fn default_stylesheet(&mut self, decl: &mut HirDeclaration) -> Result<()> {
        let HirDeclarationKind::StyleSheet {
            ref mut statements, ..
        } = decl.kind
        else {
            unreachable!("default_function should receive a function declaration");
        };

        for statement in &mut *statements {
            self.default_style_statement(statement)?;
        }
        Ok(())
    }

    ///Asserts that the given `decl` is a function and sets default values to all of its statements, and ensure its returns are correctly.
    ///If the given `decl` is not a function, a panic is going to be received, since it's obviously a bug
    pub fn default_function(&mut self, decl: &mut HirDeclaration) -> Result<()> {
        let HirDeclarationKind::Function {
            ref mut statements, ..
        } = decl.kind
        else {
            unreachable!("default_function should receive a function declaration");
        };
        let HirType::Function { return_type, .. } = self.types_module.get_type(&decl.ty).clone()
        else {
            unreachable!("A function should have function type");
        };
        let infer = self.types_module.infer_id();
        for statement in &mut *statements {
            self.default_statement(statement, &infer)?;
        }
        self.ensure_function_returns(statements, return_type, &decl.span)?;
        Ok(())
    }

    ///Sets the default types on the given `decl`. This replaces the infer types on everything on the given `decl` with the correct(or default) type
    pub fn set_default(&mut self, decl: &mut HirDeclaration) -> Result<()> {
        match decl.kind {
            HirDeclarationKind::Object | HirDeclarationKind::Alias => {}
            HirDeclarationKind::Function { .. } => self.default_function(decl)?,
            HirDeclarationKind::ComponentDeclaration { ref mut props, .. } => {
                self.resolve_component_members(props, decl.ty)?;
            }
            HirDeclarationKind::StyleSheet { .. } => self.default_stylesheet(decl)?,
        }
        Ok(())
    }
}
