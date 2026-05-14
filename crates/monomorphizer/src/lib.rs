use std::collections::{HashMap, HashSet};

use common::Span;
use slynx_hir::{Result, SlynxHir, TypeId, error::HIRError, model::HirType, modules::TypesModule};

///A struct that handles all the monomorphization on the code
pub struct Monomorphizer {
    reference_cache: HashMap<TypeId, TypeId>,
}

impl Monomorphizer {
    pub fn resolve(hir: &SlynxHir, types_module: &mut TypesModule) -> Result<()> {
        let mut this = Self {
            reference_cache: HashMap::new(),
        };
        for decl in hir.declarations.iter() {
            this.resolve_reference(hir, decl.ty, decl.span, types_module)?;
        }
        for (key, value) in this.reference_cache {
            let HirType::Reference { rf, .. } = types_module.get_type_mut(&key) else {
                continue;
            };
            *rf = value;
        }
        Ok(())
    }
    /// Resolves a reference. If the provided `id` is a reference to a concrete type, doesnt do anything, otherwise(thus, a reference)
    /// to another reference) it resolves it to make the reference point to the concrete type. This only caches it for later mutability
    pub fn resolve_reference(
        &mut self,
        _hir: &SlynxHir,
        id: TypeId,
        span: Span,
        types_module: &TypesModule,
    ) -> Result<()> {
        let mut current = id;
        let mut visited = HashSet::from([id]);
        while let HirType::Reference { rf, .. } = types_module.get_type(&current)
            && let HirType::Reference { .. } = types_module.get_type(rf)
        {
            if !visited.insert(*rf) {
                if let Some(ty) = types_module.get_type_name(&id) {
                    return Err(HIRError::recursive(*ty, span));
                } else {
                    unreachable!("Types module should have mappings to the name of a type");
                }
            }
            current = *rf;
        }
        if current != id {
            self.reference_cache.insert(id, current);
        }
        Ok(())
    }
}
