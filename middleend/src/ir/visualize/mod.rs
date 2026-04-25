use std::fmt;

use crate::{IRPointer, Instruction, InstructionType, Label, Value};

pub struct InstructionDisplay<'a> {
    pub instr: &'a Instruction,
    pub labels: &'a [Label],
    pub values: &'a [Value],
}

impl<'a> fmt::Display for InstructionDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.instr.instruction_type {

            InstructionType::Br(label_ptr) => {
                let label_str = fmt_label_ref(label_ptr, self.labels);
                let len = self.instr.operands.len();
                if len == 0 {
                    write!(f, "br {};", label_str)
                } else {
                    let args = (0..len)
                        .map(|i| fmt_value(&self.instr.operands.ptr_to(i), self.values))
                        .collect::<Vec<_>>()
                        .join(", ");
                    write!(f, "br {}({});", label_str, args)
                }
            }

            InstructionType::Cbr { then_label, else_label, then_args, else_args } => {
                let cond     = fmt_value(&self.instr.operands.ptr_to(0), self.values);
                let then_str = fmt_label_ref(then_label, self.labels);
                let else_str = fmt_label_ref(else_label, self.labels);
                let then_a   = fmt_value_range(then_args, self.values);
                let else_a   = fmt_value_range(else_args, self.values);
                write!(f, "cbr {}, {}({}), {}({});", cond, then_str, then_a, else_str, else_a)
            }

            InstructionType::Ret => {
                let val = fmt_value(&self.instr.operands.ptr_to(0), self.values);
                write!(f, "ret {};", val)
            }

            InstructionType::Add  => fmt_binary(f, "add",    self.instr, self.values),
            InstructionType::Sub  => fmt_binary(f, "sub",    self.instr, self.values),
            InstructionType::Mul  => fmt_binary(f, "mul",    self.instr, self.values),
            InstructionType::Div  => fmt_binary(f, "div",    self.instr, self.values),
            InstructionType::Cmp  => fmt_binary(f, "cmp",    self.instr, self.values),
            InstructionType::Gt   => fmt_binary(f, "cmpgt",  self.instr, self.values),
            InstructionType::Gte  => fmt_binary(f, "cmpgte", self.instr, self.values),
            InstructionType::Lt   => fmt_binary(f, "cmplt",  self.instr, self.values),
            InstructionType::Lte  => fmt_binary(f, "cmplte", self.instr, self.values),
            InstructionType::And  => fmt_binary(f, "band",   self.instr, self.values),
            InstructionType::Or   => fmt_binary(f, "bor",    self.instr, self.values),
            InstructionType::Xor  => fmt_binary(f, "bxor",   self.instr, self.values),
            InstructionType::Shl  => fmt_binary(f, "shl",    self.instr, self.values),
            InstructionType::Shr  => fmt_binary(f, "shr",    self.instr, self.values),
            InstructionType::AShr => fmt_binary(f, "ashr",   self.instr, self.values),

            _ => write!(f, "sla"),
        }
    }
}

fn fmt_label_ref(ptr: &IRPointer<Label, 1>, labels: &[Label]) -> String {
    let idx = ptr.ptr();
    let label = &labels[idx];
    if label.arguments().is_empty() {
        format!("$label_{}", idx)
    } else {
        let params = label.arguments()
            .iter()
            .map(|ty| format!("{}", ty.0))
            .collect::<Vec<_>>()
            .join(", ");
        format!("$label_{}({})", idx, params)
    }
}

fn fmt_value(ptr: &IRPointer<Value, 1>, values: &[Value]) -> String {
    match &values[ptr.ptr()] {
        Value::FuncArg(n)        => format!("p{}", n),
        Value::LabelArg(n)       => format!("lp{}", n),
        Value::Instruction(p)    => format!("%t{}", p.ptr()),
        Value::Slot(p)           => format!("%slot{}", p.ptr()),
        Value::Raw(_)            => format!("%r{}", ptr.ptr()),
        Value::StructLiteral(..) => format!("%lit{}", ptr.ptr()),
    }
}

fn fmt_value_range(ptr: &IRPointer<Value>, values: &[Value]) -> String {
    (0..ptr.len())
        .map(|i| fmt_value(&ptr.ptr_to(i), values))
        .collect::<Vec<_>>()
        .join(", ")
}

fn fmt_binary(
    f: &mut fmt::Formatter<'_>,
    op: &str,
    instr: &Instruction,
    values: &[Value],
) -> fmt::Result {
    let a = fmt_value(&instr.operands.ptr_to(0), values);
    let b = fmt_value(&instr.operands.ptr_to(1), values);
    write!(f, "{} {}, {}, {};", op, instr.value_type.0, a, b)
}

/// Returns a formatted string for a single instruction using the existing `InstructionDisplay`.
pub fn format_instruction_display(
    instr: &Instruction,
    labels: &[Label],
    values: &[Value],
) -> String {
    format!("{}", InstructionDisplay { instr, labels, values })
}

/// Formats a single label and all its instructions.
///
/// `idx` is the index of the label (used for the `$label_X` name).
/// `instructions` is the slice containing all instructions in the IR so we can index them by pointer.
pub fn format_label(
    label: &Label,
    idx: usize,
    instructions: &[Instruction],
    labels: &[Label],
    values: &[Value],
) -> String {
    let mut out = String::new();

    // Header
    if label.arguments().is_empty() {
        out.push_str(&format!("$label_{}:\n", idx));
    } else {
        let params = label
            .arguments()
            .iter()
            .map(|ty| format!("{}", ty.0))
            .collect::<Vec<_>>()
            .join(", ");
        out.push_str(&format!("$label_{}({}):\n", idx, params));
    }

    // Instructions inside the label
    let iptr = label.instruction();
    for i in 0..iptr.len() {
        let instr_ptr = iptr.ptr_to(i);
        let instr_idx = instr_ptr.ptr();
        let instr = &instructions[instr_idx];
        let line = format_instruction_display(instr, labels, values);
        out.push_str("  ");
        out.push_str(&line);
        out.push('\n');
    }

    out
}

/// Formats all labels in the IR in order, including their instructions.
pub fn format_labels(
    labels: &[Label],
    instructions: &[Instruction],
    values: &[Value],
) -> String {
    let mut out = String::new();
    for (i, label) in labels.iter().enumerate() {
        out.push_str(&format_label(label, i, instructions, labels, values));
        out.push('\n');
    }
    out
}
