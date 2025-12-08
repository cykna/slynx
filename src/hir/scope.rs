use std::collections::HashMap;

use crate::{
    hir::{
        HirId,
        error::{HIRError, HIRErrorKind},
        types::HirValue,
    },
    parser::ast::Span,
};

#[derive(Debug)]
pub struct HIRScope {
    ///The values. A map to the some value. This can be a component, property or variable. As this is not a runtime
    ///a value is simply a reference to something that on runtime will have a value.
    ///Used to track then, what kind of data it is
    values: HashMap<HirId, HirValue>,
    ///A map to a name to an id. This can be used to save variables for example
    names: HashMap<String, HirId>,
}

impl HIRScope {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            values: HashMap::new(),
        }
    }

    pub fn insert_named_value(
        &mut self,
        id: HirId,
        name: String,
        value: HirValue,
        span: &Span,
    ) -> Result<(), HIRError> {
        self.values.insert(id, value);
        if let Some(_) = self.names.get(&name) {
            Err(HIRError {
                kind: HIRErrorKind::NameAlreadyDefined(name),
                span: span.clone(),
            })
        } else {
            self.names.insert(name.clone(), id);
            Ok(())
        }
    }

    pub fn insert_value(&mut self, id: HirId, value: HirValue) {
        self.values.insert(id, value);
    }

    pub fn retrieve_value(
        &self,
        id: HirId,
        name: &str,
        span: &Span,
    ) -> Result<&HirValue, HIRError> {
        self.values.get(&id).ok_or(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }

    ///Retrieves the id of the provided `name` on the scope
    pub fn retrieve_name(&self, name: &str, span: &Span) -> Result<&HirId, HIRError> {
        self.names.get(name).ok_or(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }
}
