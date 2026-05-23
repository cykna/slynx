use slynx_hir::SlynxHir;

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub struct HirCodegen {
    hir: SlynxHir,
}

impl HirCodegen {
    pub fn new(hir: SlynxHir) -> Self {
        Self { hir }
    }
    /// Phase 0: Hoist all declarations into the IR.
    /// Creates placeholder IR types, functions, components, and style scaffolding
    /// so later phases can reference them by ID.
    fn hoist_declarations(
        &mut self,
        hir: &[HirDeclaration],
        tys: &TypesModule,
        temp: &mut TempIRData,
    ) {
        for declaration in hir {
            match &declaration.kind {
                HirDeclarationKind::Object => {
                    let out = self
                        .types
                        .create_empty_struct(*tys.get_type_name(&declaration.ty).unwrap());
                    temp.define_type(declaration.ty, out);
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
                HirDeclarationKind::Alias => {}
            }
        }
    }

    /// Pre-pass: compute property codes for all stylesheets before any lowering.
    /// This ensures parent property_codes are available regardless of declaration order.
    fn stylesheet_pre_pass(&mut self, hir: &[HirDeclaration], temp: &mut TempIRData) {
        for declaration in hir {
            if let HirDeclarationKind::StyleSheet {
                ref usages,
                ref statements,
                ..
            } = declaration.kind
            {
                let own_props = self.collect_style_properties(statements);
                let resolved = self.resolve_style_inheritance(usages, &own_props, hir);
                if let Some(style_data) = temp.get_style_mut(declaration.id) {
                    style_data.property_codes = resolved.iter().map(|rp| rp.property).collect();
                }
            }
        }
    }

    /// Phase 1: Lower all non-stylesheet declarations (Objects, Functions, Components, Aliases).
    fn lower_non_stylesheets(
        &mut self,
        hir: &[HirDeclaration],
        temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        for declaration in hir {
            match &declaration.kind {
                HirDeclarationKind::Object => {
                    self.insert_object_fields_for(declaration.ty, temp)?;
                }
                HirDeclarationKind::Function {
                    args, statements, ..
                } => {
                    self.insert_function_type_for(declaration.ty, temp)?;
                    let func = temp.get_function(declaration.id);
                    debug_assert!(func.len() == 1);
                    self.initialize_function(func.with_length::<1>(), statements, args, temp)?;
                }
                HirDeclarationKind::ComponentDeclaration { props, .. } => {
                    self.initialize_component(declaration, props, temp)?;
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
                    self.insert_stylesheet_type_for(declaration, temp)?;
                }
            }
        }
        Ok(())
    }

    /// Phase 2: Lower stylesheets in dependency order (parents before children).
    ///
    /// Uses `petgraph::algo::toposort` to compute a valid lowering order from the
    /// `uses` dependency graph.
    fn lower_stylesheets(
        &mut self,
        hir: &[HirDeclaration],
        temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        let all_stylesheets: Vec<usize> = (0..hir.len())
            .filter(|i| matches!(hir[*i].kind, HirDeclarationKind::StyleSheet { .. }))
            .collect();

        if all_stylesheets.is_empty() {
            return Ok(());
        }

        let mut graph: DiGraph<usize, ()> = DiGraph::new();
        let mut node_indices: HashMap<usize, NodeIndex<u32>> = HashMap::new();

        for &idx in &all_stylesheets {
            node_indices.insert(idx, graph.add_node(idx));
        }

        for &idx in &all_stylesheets {
            if let HirDeclarationKind::StyleSheet { ref usages, .. } = hir[idx].kind {
                for usage in usages {
                    let parent_idx = usage.style.as_raw() as usize;
                    if let Some(&parent_node) = node_indices.get(&parent_idx) {
                        graph.add_edge(parent_node, node_indices[&idx], ());
                    }
                }
            }
        }

        let order = match toposort(&graph, None) {
            Ok(order) => order.into_iter().map(|n| graph[n]).collect::<Vec<_>>(),
            Err(_) => {
                // Cycle detected: fall back to lowering all remaining stylesheets
                all_stylesheets
            }
        };

        for &idx in &order {
            self.lower_stylesheet(&hir[idx], temp)?;
        }

        Ok(())
    }
}
