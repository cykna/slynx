use common::SymbolPointer;
use slynx_hir::{
    DeclarationId,
    model::{
        HirDeclaration, HirDeclarationKind, HirExpression, HirStyleBlockKind, HirStyleStatement,
        HirStyleUsage,
    },
};

use crate::{
    IRError, IRPointer, IRTypeId, IRTypes, Instruction, SlynxIR, Value,
    ir::{
        model::Context,
        temp::TempIRData,
    },
};

/// Maps a style property name (from the stylesheet definition) to its
/// STYLES_TABLE numeric code (0..9).
pub fn style_name_to_code(name: &str) -> Option<u16> {
    match name {
        "backgroundColor" => Some(0),
        "foregroundColor" => Some(1),
        "padding" => Some(2),
        "margin" => Some(3),
        "size" => Some(4),
        "fontSize" => Some(5),
        "fontWeight" => Some(6),
        "opacity" => Some(7),
        "border" => Some(8),
        "shadow" => Some(9),
        _ => None,
    }
}

/// Map a STYLES_TABLE code to its IR type for struct fields.
pub fn style_code_to_ir_type(types: &IRTypes, code: u16) -> IRTypeId {
    match code {
        0 | 1 => types.int_type(),     // Color -> i32
        2 | 3 | 4 => types.int_type(), // Vec4 -> placeholder i32
        5 => types.int_type(),         // px -> i32
        6 => types.int_type(),         // u16 -> i32
        7 => types.float_type(),       // f32 -> f32
        8 | 9 => types.int_type(),     // Border/Shadow -> placeholder i32
        _ => types.int_type(),
    }
}

/// Collect style property definitions from a list of style statements.
/// Returns (property_name_symbol, &expression) pairs.
fn collect_style_properties<'a>(
    statements: &'a [HirStyleStatement],
) -> Vec<(SymbolPointer, &'a HirExpression)> {
    let mut props = Vec::new();
    for stmt in statements {
        if let HirStyleStatement::Styles(blocks) = stmt {
            for block in blocks {
                if !matches!(block.kind, HirStyleBlockKind::Default) {
                    continue;
                }
                for def in &block.definitions {
                    props.push((def.name, &def.expr));
                }
            }
        }
    }
    props
}

impl SlynxIR {
    /// Lower a stylesheet declaration into IR.
    ///
    /// 1. Resolve inheritance from `uses` clauses.
    /// 2. Populate the IR struct type for the style (fields in STYLES_TABLE order).
    /// 3. Create the Apply function that emits @sapply for each property.
    /// 4. Register the apply function in temp data.
    pub(crate) fn lower_stylesheet(
        &mut self,
        decl: &HirDeclaration,
        hir: &[HirDeclaration],
        temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        let HirDeclarationKind::StyleSheet {
            ref statements,
            ref usages,
            ..
        } = decl.kind
        else {
            unreachable!("lower_stylesheet called on non-stylesheet declaration");
        };

        let style_name = temp
            .types_module()
            .get_type_name(&decl.ty)
            .ok_or(IRError::IRTypeNotRecognized(decl.ty))?;

        // 1. Collect own properties from stylesheet body
        let own_props = collect_style_properties(statements);

        // 2. Resolve inheritance: merge parent properties with own properties
        let (merged_props, parent_usage_decls) =
            self.resolve_style_inheritance(usages, &own_props, hir, temp)?;

        // 3. Populate the struct type fields
        let struct_ty = temp.get_type(decl.ty)?;
        self.populate_style_struct_fields(struct_ty, &merged_props)?;

        // 4. Create the apply function
        let apply_func = self.create_style_apply_function(
            *style_name,
            struct_ty,
            &merged_props,
            &parent_usage_decls,
            temp,
        )?;

        // 5. Register the apply function so @initcall can reference it
        temp.map_style_apply_function(decl.id, apply_func);

        Ok(())
    }

    /// Resolve style inheritance.
    ///
    /// Returns merged properties in STYLES_TABLE order and parent usage declaration IDs.
    fn resolve_style_inheritance<'a>(
        &self,
        usages: &[HirStyleUsage],
        own_props: &[(SymbolPointer, &'a HirExpression)],
        hir: &'a [HirDeclaration],
        _temp: &TempIRData,
    ) -> Result<(Vec<(u16, &'a HirExpression)>, Vec<DeclarationId>), IRError> {
        let mut merged: Vec<(u16, &HirExpression)> = Vec::new();
        let mut seen_codes: Vec<u16> = Vec::new();
        let mut parent_ids: Vec<DeclarationId> = Vec::new();

        for usage in usages {
            if let Some(parent_decl) = hir.iter().find(|d| d.id == usage.style) {
                parent_ids.push(parent_decl.id);
                if let HirDeclarationKind::StyleSheet {
                    ref statements, ..
                } = parent_decl.kind
                {
                    let parent_props = collect_style_properties(statements);
                    for (_name, expr) in &parent_props {
                        let name_str = self.strings.get_name(*_name);
                        if let Some(code) = style_name_to_code(name_str) {
                            if !seen_codes.contains(&code) {
                                seen_codes.push(code);
                                merged.push((code, *expr));
                            }
                        }
                    }
                }
            }
        }

        // Own properties override parent properties with the same code
        for (name, expr) in own_props {
            let name_str = self.strings.get_name(*name);
            if let Some(code) = style_name_to_code(name_str) {
                if let Some(pos) = seen_codes.iter().position(|c| *c == code) {
                    merged[pos] = (code, *expr);
                } else {
                    seen_codes.push(code);
                    merged.push((code, *expr));
                }
            }
        }

        // Sort by STYLES_TABLE code
        merged.sort_by_key(|(code, _)| *code);

        Ok((merged, parent_ids))
    }

    /// Populate the fields of the style struct based on resolved properties.
    fn populate_style_struct_fields(
        &mut self,
        struct_ty: IRTypeId,
        properties: &[(u16, &HirExpression)],
    ) -> Result<(), IRError> {
        use crate::IRType;

        // Compute all field types first to avoid borrow conflicts
        let field_types: Vec<IRTypeId> = properties
            .iter()
            .map(|(code, _)| style_code_to_ir_type(&self.types, *code))
            .collect();

        let struct_id = match self.types.get_type(struct_ty) {
            IRType::Struct(id) => id,
            _ => unreachable!("Style struct type must be IRType::Struct"),
        };
        let struct_obj = self.types.get_object_type_mut(struct_id);

        for field_ty in field_types {
            struct_obj.insert_field(field_ty);
        }

        Ok(())
    }

    /// Create the apply function for a stylesheet.
    fn create_style_apply_function<'a>(
        &mut self,
        style_name: SymbolPointer,
        struct_ty: IRTypeId,
        properties: &[(u16, &'a HirExpression)],
        parent_ids: &[DeclarationId],
        temp: &mut TempIRData,
    ) -> Result<IRPointer<Context, 1>, IRError> {
        let apply_name = {
            let name = self.strings.get_name(style_name);
            let apply_name_str = format!("Apply{}Style", name);
            self.strings.intern(&apply_name_str)
        };

        let generic_component_ty = self.types.generic_component_type();
        let void_ty = self.types.void_type();

        // Create function context and set arg/return types
        let ctx = self.create_blank_function(apply_name);
        {
            use crate::IRType;
            let irty_id = self.get_context(ctx).ty();
            let IRType::Function(func_id) = self.types.get_type(irty_id) else {
                unreachable!();
            };
            let func_ty = self.types.get_function_type_mut(func_id);
            func_ty.insert_arg_types(&[generic_component_ty, struct_ty]);
            func_ty.set_return_type(void_ty);
        }

        // Set up entry label
        temp.set_current_function(ctx);
        let entry_label = self.insert_label(ctx, "entry");
        self.get_context_mut(ctx)
            .set_label_ptr(entry_label.with_length());
        temp.set_current_label(entry_label);

        // Insert FuncArg values for p0 (component) and p1 (struct)
        let comp_value = self.insert_value(Value::FuncArg(0));
        let struct_value = self.insert_value(Value::FuncArg(1));

        // Emit call to each parent apply function first (lower precedence first)
        for parent_id in parent_ids {
            if let Some(parent_func) = temp.get_style_apply_function(*parent_id) {
                let parent_ret_ty = self.return_type_of_context(parent_func);
                let args = {
                    let comp = self.get_value(comp_value);
                    let st = self.get_value(struct_value);
                    self.insert_values(&[comp, st])
                };
                self.insert_instruction(
                    temp.current_label(),
                    Instruction::call(parent_func, parent_ret_ty, args),
                    true,
                );
            }
        }

        // For each property, emit: getfield + @sapply
        for (field_idx, (code, _)) in properties.iter().enumerate() {
            let field_ty = self.types.get_field_type(struct_ty, field_idx);

            // getfield: extract field from struct
            let getfield_instr = self.insert_instruction(
                temp.current_label(),
                Instruction::getfield(field_idx, struct_value, field_ty),
                false,
            );
            let getfield_ptr = self.dereference_instruction_ptr(getfield_instr);
            let field_value = self.insert_value(Value::Instruction(getfield_ptr.with_length()));

            // @sapply: apply property to component
            let operands = {
                let comp = self.get_value(comp_value);
                let field = self.get_value(field_value);
                self.insert_values(&[comp, field])
            };
            let sapply_operands = operands.with_runtime_length(2);

            self.insert_instruction(
                temp.current_label(),
                Instruction::sapply(*code, sapply_operands, void_ty),
                true,
            );
        }

        // Emit ret
        let void_value = self.insert_value(Value::Void);
        self.insert_instruction(
            temp.current_label(),
            Instruction::ret(void_value, void_ty),
            true,
        );

        Ok(ctx)
    }

    /// Emit @initcall for a style usage on a component.
    pub(crate) fn emit_style_initcall(
        &mut self,
        style_usage: &HirStyleUsage,
        component_value: IRPointer<Value, 1>,
        temp: &mut TempIRData,
    ) -> Result<(), IRError> {
        let apply_func = temp
            .get_style_apply_function(style_usage.style)
            .ok_or(IRError::DeclarationNotRecognized(style_usage.style))?;

        let style_decl = temp
            .hir
            .iter()
            .find(|d| d.id == style_usage.style)
            .ok_or(IRError::DeclarationNotRecognized(style_usage.style))?;
        let struct_ty = temp.get_type(style_decl.ty)?;

        // Evaluate each parameter expression
        let mut field_values = Vec::new();
        for param in &style_usage.params {
            let val = self.get_value_for(param, temp)?;
            field_values.push(self.get_value(val));
        }

        let fields_ptr = self.insert_values(&field_values);
        let struct_lit_instr = self.insert_instruction(
            temp.current_label(),
            Instruction::struct_literal(struct_ty, fields_ptr),
            false,
        );
        let struct_lit_ptr = self.dereference_instruction_ptr(struct_lit_instr);
        let struct_lit_value =
            self.insert_value(Value::Instruction(struct_lit_ptr.with_length()));

        // Emit @initcall
        let operands = {
            let comp = self.get_value(component_value);
            let st = self.get_value(struct_lit_value);
            self.insert_values(&[comp, st])
        };
        let initcall_operands = operands.with_runtime_length(2);

        let void_ty = self.types.void_type();
        self.insert_instruction(
            temp.current_label(),
            Instruction::initcall(apply_func, initcall_operands, void_ty),
            true,
        );

        Ok(())
    }
}
