pub mod expr;
use std::collections::HashMap;

use crate::hir::{
    HirId, SlynxHir,
    declaration::{
        ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirStatment, HirStatmentKind,
    },
    types::HirType,
};

pub struct Monomorphizer {
    ///Elements that contains generic and so require to be monomorphized
    base_elements: HashMap<HirId, HirDeclaration>,
    ///Elements of raw structs that are currently monomorphized
    elements: Vec<HirDeclaration>,
}

impl Monomorphizer {
    ///Creates a new monomorphizer
    pub fn new() -> Self {
        Self {
            base_elements: HashMap::new(),
            elements: Vec::new(),
        }
    }

    pub fn register_element(&mut self, decl: HirDeclaration) {
        self.base_elements.insert(decl.id, decl);
    }

    ///Tries to monomorphize the values inside a element deffinition
    pub fn try_monomorphize_values(&mut self, values: &mut Vec<ElementValueDeclaration>) {
        for value in values {
            match value {
                ElementValueDeclaration::Child { name, values, span } => {}
                ElementValueDeclaration::Property {
                    id,
                    index,
                    value,
                    span,
                } => {}
                _ => {}
            }
        }
    }

    pub fn try_monomorphize_statements(&mut self, statments: &mut Vec<HirStatment>) {
        for statment in statments {
            match statment.kind {
                HirStatmentKind::Expression { ref mut expr }
                | HirStatmentKind::Return { ref mut expr } => {
                    self.monomorphize_expr(expr);
                }
            }
        }
    }

    ///Tries to monomorphize the provided `decl` using the provided `hir` as the context to get info of the values that would require the monomorphization
    pub fn monomorphize_declaration(
        &mut self,
        mut decl: HirDeclaration,
        hir: &SlynxHir,
    ) -> Option<HirDeclaration> {
        match decl.kind {
            HirDeclarationKind::ElementDeclaration { ref mut props } => {
                let HirType::Component { props: tys } = &decl.ty else {
                    unreachable!();
                };
                for ty in tys {
                    if matches!(ty.2, HirType::Generic(_)) {
                        self.try_monomorphize_values(props);
                        self.register_element(decl);
                        return None;
                    }
                }
                Some(decl)
            }
            HirDeclarationKind::Function {
                ref mut statments, ..
            } => {
                self.try_monomorphize_statements(statments);
                Some(decl)
            }
        }
    }

    pub fn monomorphize(mut self, mut hir: SlynxHir) -> Vec<HirDeclaration> {
        let mut monomorphized_decls = Vec::new();
        for decl in std::mem::take(&mut hir.declarations) {
            if let Some(monomorphized_decl) = self.monomorphize_declaration(decl, &hir) {
                monomorphized_decls.push(monomorphized_decl);
            }
        }
        self.elements.extend_from_slice(&monomorphized_decls);
        self.elements
    }
}
