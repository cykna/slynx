use common::SymbolsModule;
use slynx_hir::{
    model::{HirDeclaration, HirDeclarationKind, HirType},
    modules::TypesModule,
};

use crate::{
    AuxiliaryStyle, Component, Context, Formatter, IRError, IRPointer, IRTypes, Instruction, Label,
    Operand, Slot, TempIRData, Value,
};

#[derive(Debug)]
///All the IR containing contexts, labels, instructions and operands
pub struct SlynxIR {
    ///The contexts of this IR
    pub(crate) contexts: Vec<Context>,
    ///The Components of this IR
    pub(crate) components: Vec<Component>,
    ///The labels of this IR
    pub(crate) labels: Vec<Label>,
    ///The instructions of this IR
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) instruction_pointers: Vec<IRPointer<Instruction>>,
    ///The operands of this IR
    pub(crate) operands: Vec<Operand>,
    ///The values of this IR
    pub(crate) values: Vec<Value>,
    pub(crate) slots: Vec<Slot>,
    pub(crate) types: IRTypes,
    ///Pool of interned strings, accessed via StringHandle indices
    pub(crate) strings: SymbolsModule,
}

impl SlynxIR {
    ///Creates a new empty IR
    pub fn new(symbols: SymbolsModule) -> Self {
        Self {
            components: Vec::new(),
            contexts: Vec::new(),
            labels: Vec::new(),
            instructions: Vec::new(),
            instruction_pointers: Vec::new(),
            operands: Vec::new(),
            values: Vec::new(),
            slots: Vec::new(),
            types: IRTypes::new(),
            strings: symbols,
        }
    }

    ///Returns a reference to the string pool
    pub fn string_pool(&self) -> &SymbolsModule {
        &self.strings
    }

    ///Generates all the code on the IR, with types, functions, lowerings, etc, based on the provided `hir`. The `tys` is expected to be the types module used by the `hir` during all frontend process, as well as
    ///the `symbols`, to be the symbols module used by the same `hir` during all the frontend process
    pub fn generate(&mut self, hir: Vec<HirDeclaration>, tys: &TypesModule) -> Result<(), IRError> {
        let mut temp = TempIRData::new(tys, &hir);
        //hoist of the objects
        for declaration in &hir {
            match &declaration.kind {
                HirDeclarationKind::Object => {
                    let out = self
                        .types
                        .create_empty_struct(*tys.get_type_name(&declaration.ty).unwrap());
                    temp.define_type(declaration.ty, out);
                    // Also map the inner unnamed Struct TypeId (the `rf` of the Reference)
                    if let HirType::Reference { rf, .. } = tys.get_type(&declaration.ty) {
                        temp.define_type(*rf, out);
                    }
                }
                HirDeclarationKind::Function { name, .. } => {
                    let out = self.create_blank_function(*name).with_length();
                    let ctx = self.get_context(out);
                    temp.define_type(declaration.ty, ctx.ty());
                    temp.map_function(declaration.id, out.with_length());
                }
                HirDeclarationKind::ComponentDeclaration { name, .. } => {
                    let out = self.create_blank_component(*name);
                    let fnc = self.get_component(out);
                    temp.define_type(declaration.ty, fnc.ty);
                    temp.map_component(declaration.id, out);

                    let name_str = self.strings.get_name(*name);
                    let init_name = self.strings.intern(&format!("{}Init", name_str));
                    let init_func = self.create_blank_function(init_name);
                    temp.map_init_function(declaration.id, init_func);
                }
                HirDeclarationKind::Alias => {}
                HirDeclarationKind::StyleSheet { .. } => {
                    if let Some(name) = tys.get_type_name(&declaration.ty).cloned() {
                        let name_str = self.strings.get_name(name);
                        let init_name = self.strings.intern(&format!("__init_{}", name_str));

                        let out = self.types.create_empty_struct(name);
                        let init_func = self.create_blank_function(init_name);
                        let apply_func = self.create_blank_function(name);

                        temp.define_type(declaration.ty, out);
                        temp.map_style(
                            declaration.id,
                            AuxiliaryStyle {
                                init_func: init_func.with_length(),
                                apply_func: apply_func.with_length(),
                                strct: out,
                                property_codes: Vec::new(),
                            },
                        );
                    }
                }
            }
        }
        // Pre-pass: compute property codes for all stylesheets before any lowering.
        // This ensures parent property_codes are available regardless of declaration order.
        for declaration in &hir {
            if let HirDeclarationKind::StyleSheet {
                ref usages,
                ref statements,
                ..
            } = declaration.kind
            {
                let own_props = self.collect_style_properties(statements);
                let resolved = self.resolve_style_inheritance(usages, &own_props, &hir);
                if let Some(style_data) = temp.get_style_mut(declaration.id) {
                    style_data.property_codes = resolved.iter().map(|rp| rp.property).collect();
                }
            }
        }

        // Phase 1: Lower all non-stylesheet declarations (Objects, Functions, Components, Aliases)
        for declaration in &hir {
            match &declaration.kind {
                HirDeclarationKind::Object => {
                    self.insert_object_fields_for(declaration.ty, &temp)?;
                }
                HirDeclarationKind::Function {
                    args, statements, ..
                } => {
                    self.insert_function_type_for(declaration.ty, &temp)?;
                    let func = temp.get_function(declaration.id);
                    debug_assert!(func.len() == 1);
                    self.initialize_function(func.with_length::<1>(), statements, args, &mut temp)?;
                }
                HirDeclarationKind::ComponentDeclaration { props, .. } => {
                    self.initialize_component(declaration, props, &mut temp)?;
                }
                HirDeclarationKind::Alias => {
                    let HirType::Reference { rf, .. } =
                        temp.types_module().get_type(&declaration.ty)
                    else {
                        unreachable!("Declaration type of alias is always reference")
                    };

                    let t = temp.get_type(*rf)?;

                    temp.define_type(declaration.ty, t);
                }
                HirDeclarationKind::StyleSheet { .. } => {
                    self.insert_stylesheet_type_for(declaration, &temp)?;
                }
            }
        }

        // Phase 2: Lower stylesheets in dependency order (parents before children)
        {
            use std::collections::HashSet;
            let mut lowered: HashSet<usize> = HashSet::new();
            let all_stylesheets: Vec<usize> = (0..hir.len())
                .filter(|i| matches!(hir[*i].kind, HirDeclarationKind::StyleSheet { .. }))
                .collect();

            while lowered.len() < all_stylesheets.len() {
                let before = lowered.len();
                for &idx in &all_stylesheets {
                    if lowered.contains(&idx) {
                        continue;
                    }
                    let decl = &hir[idx];
                    if let HirDeclarationKind::StyleSheet { ref usages, .. } = decl.kind {
                        let all_parents_lowered = usages.iter().all(|u| {
                            let pidx = u.style.as_raw() as usize;
                            lowered.contains(&pidx)
                                || !matches!(hir[pidx].kind, HirDeclarationKind::StyleSheet { .. })
                        });
                        if all_parents_lowered {
                            self.lower_stylesheet(decl, &mut temp)?;
                            lowered.insert(idx);
                        }
                    }
                }
                // If no progress, lower remaining (handles cycles gracefully)
                if lowered.len() == before {
                    for &idx in &all_stylesheets {
                        if !lowered.contains(&idx) {
                            self.lower_stylesheet(&hir[idx], &mut temp)?;
                            lowered.insert(idx);
                        }
                    }
                    break;
                }
            }
        }

        Ok(())
    }

    /// Produces a Slynx IR textual dump (SIR) following the README syntax.
    ///
    /// This uses the helpers defined in the `visualize` module to format labels and
    /// instructions in the human-readable SIR form described in `middleend/README.md`.
    pub fn format_sir(&self) -> String {
        let fmt = Formatter::new(self, &self.strings);
        let mut out = fmt.format_types();
        out.push_str(&fmt.format_contexts());
        out
    }
}
