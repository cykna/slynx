use color_eyre::eyre::Result;

use crate::{
    hir::{
        SlynxHir,
        error::{HIRError, HIRErrorKind},
        symbols::SymbolPointer,
    },
    parser::ast::Span,
};

//file specific to implement things related to name resolution
impl SlynxHir {
    pub fn insert_name(&mut self, name: &str) -> super::symbols::SymbolPointer {
        self.symbols_module.intern(name)
    }
    ///Retrieves the pointer(simply a symbol) of the provided `name`.
    pub fn get_symbol_of(&self, name: &str, span: &Span) -> Result<SymbolPointer> {
        self.symbols_module.retrieve(name).cloned().ok_or(
            HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            }
            .into(),
        )
    }
}
