use std::collections::{HashMap, HashSet};

use crate::{
    hir::{
        HirId,
        error::{HIRError, HIRErrorKind},
    },
    parser::ast::Span,
};

#[derive(Debug)]
pub struct HIRScope {
    ///A map to a name to an id. This can be used to save variables for example
    names: HashMap<String, HirId>,
    mutables: HashSet<HirId>,
}

impl HIRScope {
    pub fn new() -> Self {
        Self {
            mutables: HashSet::new(),
            names: HashMap::new(),
        }
    }

    pub fn insert_name(&mut self, id: HirId, name: String) {
        self.names.insert(name, id);
    }

    ///Defines that the provided `id` is mutable, generally for variable names
    pub fn set_mutable(&mut self, id: HirId) {
        self.mutables.insert(id);
    }

    ///Retrieves the id of the provided `name` on the scope
    pub fn retrieve_name(&self, name: &str, span: &Span) -> Result<&HirId, HIRError> {
        self.names.get(name).ok_or(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        })
    }
}
