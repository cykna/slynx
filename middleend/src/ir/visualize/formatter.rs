use std::collections::{BTreeSet, HashMap, HashSet};

use common::SymbolsModule;

use crate::{
    Component, Context, IRComponentId, IRPointer, IRSpecializedComponent,
    IRSpecializedComponentType, IRStructId, IRType, IRTypes, Instruction, InstructionType, Label,
    Operand, Slot, Value,
};

/// Formatter holds the slices and helpers necessary to render the IR in textual form (SIR).
/// It provides instance methods that replace the free functions previously living in the
/// `visualize` module.
pub struct Formatter<'a> {
    pub labels: &'a [Label],
    pub contexts: &'a [Context],
    pub components: &'a [Component],
    pub values: &'a [Value],
    pub operands: &'a [Operand],
    pub types: &'a IRTypes,
    pub symbols: &'a SymbolsModule,
    pub specialized: &'a [IRSpecializedComponent],
    pub instruction_pointers: &'a [IRPointer<Instruction>],
    pub instructions: &'a [Instruction],
    label_offset: usize,
    /// Indices of unmapped instructions that should be inlined (used exactly once, simple op).
    inline_set: HashSet<usize>,
}

impl<'a> Formatter<'a> {
    /// Create a new formatter instance.
    pub fn new(
        labels: &'a [Label],
        contexts: &'a [Context],
        components: &'a [Component],
        values: &'a [Value],
        operands: &'a [Operand],
        types: &'a IRTypes,
        symbols: &'a SymbolsModule,
        specialized: &'a [IRSpecializedComponent],
        instruction_pointers: &'a [IRPointer<Instruction>],
        instructions: &'a [Instruction],
    ) -> Self {
        Self {
            contexts,
            components,
            labels,
            values,
            operands,
            types,
            symbols,
            specialized,
            instruction_pointers,
            instructions,
            label_offset: 0,
            inline_set: HashSet::new(),
        }
    }

    fn with_label_offset(&self, offset: usize) -> Formatter<'a> {
        Formatter { label_offset: offset, inline_set: HashSet::new(), ..*self }
    }

    /// Returns true if the instruction at `instr_idx` (index into `instructions`) is mapped.
    fn is_mapped(&self, instr_idx: usize) -> bool {
        self.instruction_pointers.iter().any(|p| p.ptr() == instr_idx)
    }

    /// Returns true if an unmapped instruction is simple enough to be inlined.
    fn is_inlinable(instr: &Instruction) -> bool {
        matches!(
            instr.instruction_type,
            InstructionType::Component | InstructionType::Struct
        )
    }

    /// Counts how many times each unmapped instruction index is referenced by the operands
    /// of `instr_idx` (recursively), excluding RawValue which are always inlined.
    fn count_refs(&self, instr_idx: usize, counts: &mut HashMap<usize, usize>) {
        let instr = &self.instructions[instr_idx];
        for i in 0..instr.operands.len() {
            if let Value::Instruction(p) = &self.values[instr.operands.ptr_to(i).ptr()] {
                let dep = p.ptr();
                let dep_instr = &self.instructions[dep];
                if matches!(dep_instr.instruction_type, InstructionType::RawValue) {
                    continue;
                }
                if !self.is_mapped(dep) {
                    *counts.entry(dep).or_insert(0) += 1;
                    if *counts.get(&dep).unwrap() == 1 {
                        self.count_refs(dep, counts);
                    }
                }
            }
        }
    }

    /// Builds the inline_set for a label: unmapped instructions referenced exactly once
    /// that are simple enough to inline.
    fn build_inline_set(&self, label: &Label) -> HashSet<usize> {
        let iptr = label.instructions();
        let mut counts: HashMap<usize, usize> = HashMap::new();
        for i in 0..iptr.len() {
            let ip_idx = iptr.ptr_to(i).ptr();
            let real_idx = self.instruction_pointers[ip_idx].ptr();
            self.count_refs(real_idx, &mut counts);
        }
        counts
            .into_iter()
            .filter(|(idx, count)| *count == 1 && Self::is_inlinable(&self.instructions[*idx]))
            .map(|(idx, _)| idx)
            .collect()
    }

    fn fmt_struct(&self, t: &IRStructId) -> String {
        let strukt = self.types.get_object_type(*t);
        if let Some(name) = strukt.name() {
            format!("%{}", self.symbols.get_name(name))
        } else {
            let fields = self
                .types
                .get_object_type(*t)
                .get_fields()
                .iter()
                .map(|v| {
                    let t = self.types.get_type(*v);
                    self.fmt_type(&t)
                })
                .collect::<Vec<_>>()
                .join(",");
            format!("{{{fields}}}")
        }
    }

    fn fmt_component(&self, t: &IRComponentId) -> String {
        let component = self.types.get_component_type(*t);
        format!("%{}", self.symbols.get_name(component.name()))
    }
    /// Convert IRType into a human friendly string. Kept as a free fn for reuse inside the module.
    fn fmt_type(&self, ty: &IRType) -> String {
        match ty {
            IRType::I8 => "i8".to_string(),
            IRType::U8 => "u8".to_string(),
            IRType::I16 => "i16".to_string(),
            IRType::U16 => "u16".to_string(),
            IRType::I32 => "i32".to_string(),
            IRType::U32 => "u32".to_string(),
            IRType::I64 => "i64".to_string(),
            IRType::U64 => "u64".to_string(),
            IRType::F32 => "f32".to_string(),
            IRType::F64 => "f64".to_string(),
            IRType::BOOL => "bool".to_string(),
            IRType::VOID => "void".to_string(),
            IRType::STR => "str".to_string(),
            IRType::Struct(t) => self.fmt_struct(t),
            IRType::Component(c) => self.fmt_component(c),
            IRType::Function(_) => "fn".to_string(),
            IRType::GenericComponent => "component".to_string(),
            IRType::ISIZE => "isize".to_string(),
            IRType::USIZE => "usize".to_string(),
            IRType::Specialized(IRSpecializedComponentType::Div) => "specialized(div)".to_string(),
            IRType::Specialized(IRSpecializedComponentType::Text) => {
                "specialized(text)".to_string()
            }
        }
    }

    pub fn format_contexts(&self) -> String {
        let mut out = Vec::new();
        for ctx in self.contexts {
            let ty = ctx.ty();
            let IRType::Function(fty) = self.types.get_type(ty) else {
                unreachable!("Type of context should be function");
            };
            let ty = self.types.get_function_type(fty);
            let ty = self.fmt_type(&self.types.get_type(ty.get_return_type()));
            let mut current = format!("{ty} {}(){{\n", self.symbols.get_name(ctx.name()));
            let labels_ptr = ctx.labels_ptr();
            let label_offset = labels_ptr.ptr();
            let range = labels_ptr.range();
            let fmt = self.with_label_offset(label_offset);
            let mut idx = 0;
            for label in &self.labels[range.start..range.end] {
                current.push_str(&fmt.format_label(label, idx));
                idx += 1;
            }
            current.push_str("}\n");
            out.push(current);
        }

        for component in self.components {
            let ty = component.ir_type();
            let IRType::Component(cid) = self.types.get_type(ty) else {
                unreachable!("Type of context should be component");
            };
            let comp_ty = self.types.get_component_type(cid);
            let params = comp_ty
                .fields()
                .iter()
                .map(|v| self.fmt_type(&self.types.get_type(*v)))
                .collect::<Vec<_>>()
                .join(", ");

            let mut current = format!(
                "component %{}({}) {{\n",
                self.symbols.get_name(comp_ty.name()),
                params
            );

            // Props: %fieldN: ty = pN
            for (i, field_ty) in comp_ty.fields().iter().enumerate() {
                let ty_str = self.fmt_type(&self.types.get_type(*field_ty));
                current.push_str(&format!("  %field{}: {} = p{};\n", i, ty_str, i));
            }

            // Children: #tN: specialized <type>
            let values_range = component.values();
            for i in 0..values_range.len() {
                let val = &self.values[values_range.ptr_to(i).ptr()];
                let child_str = match val {
                    Value::Instruction(instr_ptr) => {
                        format!("#t{}: component %t{}", i, instr_ptr.ptr())
                    }
                    Value::Specliazed(spec_ptr) => {
                        let spec = &self.specialized[spec_ptr.ptr()];
                        let kind = match spec {
                            IRSpecializedComponent::Text(_) => "specialized(text)",
                            IRSpecializedComponent::Div(_) => "specialized(div)",
                        };
                        format!("#t{}: {}", i, kind)
                    }
                    _ => format!("#t{}: unknown", i),
                };
                current.push_str(&format!("  {};\n", child_str));
            }

            current.push_str("}\n");
            out.push(current);
        }
        out.join("\n")
    }

    pub fn format_types(&self) -> String {
        let mut out = String::new();
        for (name, fields) in self
            .types
            .structs()
            .iter()
            .filter_map(|s| s.name().map(|name| (name, s.get_fields())))
        {
            let fields = fields
                .iter()
                .map(|f| self.fmt_type(&self.types.get_type(*f)))
                .collect::<Vec<_>>()
                .join(",");
            let v = format!("struct %{}{{{}}};\n", self.symbols.get_name(name), fields);
            out.push_str(&v);
        }
        out
    }

    /// Recursively collects indices (into `instructions`) of unmapped instructions
    /// that `instr_idx` depends on, in ascending order, into `out`.
    fn collect_unmapped_deps(&self, instr_idx: usize, out: &mut BTreeSet<usize>) {
        let instr = &self.instructions[instr_idx];
        for i in 0..instr.operands.len() {
            let val_ptr = instr.operands.ptr_to(i);
            if let Value::Instruction(p) = &self.values[val_ptr.ptr()] {
                let dep_idx = p.ptr();
                let dep_instr = &self.instructions[dep_idx];
                if matches!(dep_instr.instruction_type, InstructionType::RawValue) {
                    continue;
                }
                if self.inline_set.contains(&dep_idx) {
                    continue; // will be inlined, no need to emit as own line
                }
                if !self.is_mapped(dep_idx) && out.insert(dep_idx) {
                    self.collect_unmapped_deps(dep_idx, out);
                }
            }
        }
    }

    pub fn format_label(&self, label: &Label, idx: usize) -> String {
        let mut out = String::new();

        if label.arguments().is_empty() {
            out.push_str(&format!("$label_{}:\n", idx));
        } else {
            let params = label
                .arguments()
                .iter()
                .enumerate()
                .map(|(i, _)| format!("lp{}", i))
                .collect::<Vec<_>>()
                .join(", ");
            out.push_str(&format!("$label_{}({}):\n", idx, params));
        }

        let inline_set = self.build_inline_set(label);
        let fmt = Formatter { inline_set, ..*self };

        let iptr = label.instructions();
        let mut emitted: BTreeSet<usize> = BTreeSet::new();

        for i in 0..iptr.len() {
            let ip_idx = iptr.ptr_to(i).ptr();
            let real_idx = fmt.instruction_pointers[ip_idx].ptr();

            let mut deps = BTreeSet::new();
            fmt.collect_unmapped_deps(real_idx, &mut deps);
            for dep_idx in deps {
                if emitted.insert(dep_idx) {
                    let dep = &fmt.instructions[dep_idx];
                    let line = fmt.format_instruction(dep);
                    out.push_str(&format!("  %t{} = {}\n", dep_idx, line));
                }
            }

            let instr = &fmt.instructions[real_idx];
            let line = fmt.format_instruction(instr);
            let produces_value = !matches!(
                instr.instruction_type,
                InstructionType::Br(_)
                    | InstructionType::Cbr { .. }
                    | InstructionType::Write(_)
                    | InstructionType::Ret
            );
            out.push_str("  ");
            if produces_value {
                out.push_str(&format!("%t{} = {}", real_idx, line));
            } else {
                out.push_str(&line);
            }
            out.push('\n');
        }

        out
    }

    pub fn format_labels(&self) -> String {
        let mut out = String::new();
        for (i, label) in self.labels.iter().enumerate() {
            out.push_str(&self.format_label(label, i));
            out.push('\n');
        }
        out
    }
    /// Format a single instruction into a String (the full textual representation).
    pub fn format_instruction(&self, instr: &Instruction) -> String {
        match &instr.instruction_type {
            InstructionType::Br(label_ptr) => {
                let label_str = self.fmt_label_ref(label_ptr);
                let len = instr.operands.len();

                if len == 0 {
                    format!("br {};", label_str)
                } else {
                    let args = (0..len)
                        .map(|i| self.fmt_value(&instr.operands.ptr_to(i)))
                        .collect::<Vec<_>>()
                        .join(", ");
                    if !args.is_empty() {
                        format!("br {}({});", label_str, args)
                    } else {
                        format!("br {};", label_str)
                    }
                }
            }

            InstructionType::Cbr {
                then_label,
                else_label,
                then_args,
                else_args,
            } => {
                let cond = self.fmt_value(&instr.operands.ptr_to(0));
                let then_str = self.fmt_label_ref(then_label);
                let else_str = self.fmt_label_ref(else_label);
                let then_a = self.fmt_value_range(then_args);
                let else_a = self.fmt_value_range(else_args);
                format!(
                    "cbr {}, {}({}), {}({});",
                    cond, then_str, then_a, else_str, else_a
                )
            }

            InstructionType::Ret => {
                let val = self.fmt_value(&instr.operands.ptr_to(0));
                format!("ret {};", val)
            }

            InstructionType::Add => self.fmt_binary("add", instr),
            InstructionType::Sub => self.fmt_binary("sub", instr),
            InstructionType::Mul => self.fmt_binary("mul", instr),
            InstructionType::Div => self.fmt_binary("div", instr),
            InstructionType::Cmp => self.fmt_binary("cmp", instr),
            InstructionType::Gt => self.fmt_binary("cmpgt", instr),
            InstructionType::Gte => self.fmt_binary("cmpgte", instr),
            InstructionType::Lt => self.fmt_binary("cmplt", instr),
            InstructionType::Lte => self.fmt_binary("cmplte", instr),
            InstructionType::And => self.fmt_binary("band", instr),
            InstructionType::Or => self.fmt_binary("bor", instr),
            InstructionType::Xor => self.fmt_binary("bxor", instr),
            InstructionType::Shl => self.fmt_binary("shl", instr),
            InstructionType::Shr => self.fmt_binary("shr", instr),
            InstructionType::AShr => self.fmt_binary("ashr", instr),

            InstructionType::GetField(u) => self.fmt_get_field(instr, *u),
            InstructionType::SetField(u) => self.fmt_set_field(instr, *u),
            InstructionType::FunctionCall(temp) => self.fmt_function_call(instr, temp),

            InstructionType::Allocate(_) => self.fmt_allocate(instr),
            InstructionType::Write(slot) => self.fmt_write(instr, slot),
            InstructionType::Read => self.fmt_read(instr),
            InstructionType::Reinterpret => self.fmt_reinterpret(instr),

            InstructionType::RawValue => {
                let ptr = instr.operands.ptr_to(0);
                self.fmt_value(&ptr)
            }
            InstructionType::Struct => {
                let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
                let args = (0..instr.operands.len())
                    .map(|i| self.fmt_value(&instr.operands.ptr_to(i)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", ty_str, args)
            }
            InstructionType::Component => {
                let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
                let args = (0..instr.operands.len())
                    .map(|i| self.fmt_value(&instr.operands.ptr_to(i)))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("{}({})", ty_str, args)
            }
        }
    }

    #[inline]
    fn fmt_label_ref(&self, ptr: &IRPointer<Label, 1>) -> String {
        let idx = ptr.ptr().saturating_sub(self.label_offset);
        format!("$label_{}", idx)
    }

    fn fmt_get_field(&self, instr: &Instruction, index: usize) -> String {
        let target = self.fmt_value(&instr.operands.ptr_to(0));
        let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
        format!("getfield {}, {}, {};", ty_str, target, index)
    }

    fn fmt_set_field(&self, instr: &Instruction, slot: usize) -> String {
        let target = self.fmt_value(&instr.operands.ptr_to(0));
        // slot here is an index into struct fields (not an IRPointer)
        let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
        format!("setfield {}, {}, {};", ty_str, target, slot)
    }

    fn fmt_function_call(&self, instr: &Instruction, func: &IRPointer<Context, 1>) -> String {
        let args = (0..instr.operands.len())
            .map(|i| self.fmt_value(&instr.operands.ptr_to(i)))
            .collect::<Vec<_>>()
            .join(", ");

        let fname = format!("f{}", func.ptr());

        format!("call {}, {};", fname, args)
    }

    fn fmt_allocate(&self, instr: &Instruction) -> String {
        format!(
            "allocate {};",
            self.fmt_type(&self.types.get_type(instr.value_type))
        )
    }

    fn fmt_write(&self, instr: &Instruction, slot: &IRPointer<Slot, 1>) -> String {
        let value = self.fmt_value(&instr.operands.ptr_to(0));
        let slot_str = format!("${}", slot.ptr());
        let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
        format!("write {}, {}, {};", ty_str, slot_str, value)
    }

    fn fmt_read(&self, instr: &Instruction) -> String {
        let slot = instr.operands.ptr_to(0);
        let slot_str = self.fmt_value(&slot);
        let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
        format!("read {}, {};", ty_str, slot_str)
    }

    fn fmt_reinterpret(&self, instr: &Instruction) -> String {
        let slot_str = self.fmt_value(&instr.operands.ptr_to(0));
        let ty_str = self.fmt_type(&self.types.get_type(instr.value_type));
        format!("reinterpret {}, {};", ty_str, slot_str)
    }

    fn fmt_value(&self, ptr: &IRPointer<Value, 1>) -> String {
        match &self.values[ptr.ptr()] {
            Value::FuncArg(n) => format!("p{}", n),
            Value::LabelArg(n) => format!("lp{}", n),
            Value::Instruction(p) => {
                let instr = &self.instructions[p.ptr()];
                if matches!(instr.instruction_type, InstructionType::RawValue) {
                    self.fmt_value(&instr.operands.ptr_to(0))
                } else if self.inline_set.contains(&p.ptr()) {
                    self.format_instruction(instr).trim_end_matches(';').to_string()
                } else {
                    format!("%t{}", p.ptr())
                }
            }
            Value::Slot(p) => format!("${}", p.ptr()),
            Value::Raw(op_ptr) => {
                let op = &self.operands[op_ptr.ptr()];
                match op {
                    Operand::Bool(b) => b.to_string(),
                    Operand::Int(i) => i.to_string(),
                    Operand::Float(f) => f.to_string(),
                    Operand::String(sym) => format!("\"{}\"", self.symbols.get_name(*sym)),
                }
            }
            Value::StructLiteral(_, _) => format!("%lit{}", ptr.ptr()),
            Value::Void => "void".to_string(),
            Value::Specliazed(_) => "Specialized not implemented".to_string(),
        }
    }

    fn fmt_value_range(&self, ptr: &IRPointer<Value>) -> String {
        (0..ptr.len())
            .map(|i| self.fmt_value(&ptr.ptr_to(i)))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn fmt_binary(&self, op: &str, instr: &Instruction) -> String {
        let a = self.fmt_value(&instr.operands.ptr_to(0));
        let b = self.fmt_value(&instr.operands.ptr_to(1));
        let sla = self.types.get_type(instr.value_type);
        let ty_str = self.fmt_type(&sla);
        format!("{} {}, {}, {};", op, ty_str, a, b)
    }
}
