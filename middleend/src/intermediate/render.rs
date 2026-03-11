use std::collections::HashMap;

use crate::intermediate::{
    IntermediateRepr,
    context::{IntermediateContext, IntermediateContextType},
    expr::{IntermediateExpr, IntermediateExprKind, NativeComponentKind},
    id::{ContextHandle, TyId, ValueId, VarId},
    node::{IntermediateInstruction, IntermediateInstructionKind, IntermediatePlace},
    types::IntermediateType,
};
use common::ast::Operator;

impl IntermediateRepr {
    pub fn render_text(&self) -> String {
        let mut out = String::from("ir {\n");

        out.push_str("  struct %StrHandle {usize, usize};\n");

        if !self.strings.is_empty() {
            out.push('\n');
            for (idx, handle) in self.strings.handles().iter().enumerate() {
                out.push_str(&format!("  @str{idx} = {:?};\n", &self.strings[handle]));
            }
        }

        let mut type_ids = self.types_mapping.keys().copied().collect::<Vec<_>>();
        type_ids.sort_by_key(|ty| ty.as_raw());
        let mut rendered_struct = false;
        for ty_id in type_ids {
            if self.is_component_type(ty_id) {
                continue;
            }
            let Some(IntermediateType::Complex(fields)) = self.types_mapping.get(&ty_id) else {
                continue;
            };
            if !rendered_struct {
                out.push('\n');
                rendered_struct = true;
            }
            out.push_str(&format!(
                "  struct {} {{{}}};\n",
                self.render_ty_id(ty_id),
                fields
                    .iter()
                    .map(|field| self.render_type(field))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }

        if !self.contexts.is_empty() {
            out.push('\n');
        }

        for (idx, context) in self.contexts.iter().enumerate() {
            if idx != 0 {
                out.push('\n');
            }
            self.render_context(context, &mut out);
        }

        out.push_str("}\n");
        out
    }

    fn render_context(&self, context: &IntermediateContext, out: &mut String) {
        match &context.ty {
            IntermediateContextType::Function {
                name,
                args,
                ret,
                instructions,
            } => {
                let labels = self.variable_labels(context);
                let args = args
                    .iter()
                    .enumerate()
                    .map(|(idx, (ty, _))| format!("p{idx}: {}", self.render_type(ty)))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!(
                    "  {} #{}({}) {{\n",
                    self.render_type(ret),
                    name,
                    args
                ));
                self.render_values(context, &labels, out);
                self.render_code(instructions, &labels, out);
                out.push_str("  }\n");
            }
            IntermediateContextType::Component {
                name,
                properties,
                children,
            } => {
                let prop_types = properties
                    .iter()
                    .map(|property| self.render_type(&property.ty))
                    .collect::<Vec<_>>()
                    .join(", ");
                out.push_str(&format!("  component %{}({}) {{\n", name, prop_types));
                for property in properties {
                    let default = property
                        .default_value
                        .map(|value| format!(" = {}", self.render_value_ref(value)))
                        .unwrap_or_default();
                    out.push_str(&format!(
                        "    %{}: {}{};\n",
                        property.name,
                        self.render_type(&property.ty),
                        default
                    ));
                }
                self.render_values(context, &HashMap::new(), out);
                if !children.is_empty() {
                    out.push_str(&format!(
                        "    children [{}];\n",
                        children
                            .iter()
                            .map(|child| self.render_value_ref(*child))
                            .collect::<Vec<_>>()
                            .join(", ")
                    ));
                }
                out.push_str("  }\n");
            }
        }
    }

    fn render_values(
        &self,
        context: &IntermediateContext,
        labels: &HashMap<VarId, String>,
        out: &mut String,
    ) {
        if context.exprs.is_empty() {
            return;
        }

        out.push_str("    values {\n");
        for (idx, expr) in context.exprs.iter().enumerate() {
            out.push_str(&format!(
                "      {} = {};\n",
                self.render_value_ref(ValueId::from_raw(idx as u64)),
                self.render_expr(expr, labels)
            ));
        }
        out.push_str("    }\n");
    }

    fn render_code(
        &self,
        instructions: &[IntermediateInstruction],
        labels: &HashMap<VarId, String>,
        out: &mut String,
    ) {
        if instructions.is_empty() {
            return;
        }

        out.push_str("    code {\n");
        for instruction in instructions {
            out.push_str(&format!(
                "      {};\n",
                self.render_instruction(instruction, labels)
            ));
        }
        out.push_str("    }\n");
    }

    fn render_instruction(
        &self,
        instruction: &IntermediateInstruction,
        labels: &HashMap<VarId, String>,
    ) -> String {
        match &instruction.kind {
            IntermediateInstructionKind::Alloc(var) => {
                format!("alloc {}", self.render_var(*var, labels))
            }
            IntermediateInstructionKind::Move { target, value } => format!(
                "mov {}, {}",
                self.render_place(target, labels),
                self.render_value_ref(*value)
            ),
            IntermediateInstructionKind::Read(var) => {
                format!("read {}", self.render_var(*var, labels))
            }
            IntermediateInstructionKind::Ret(value) => {
                format!("ret {}", self.render_value_ref(*value))
            }
        }
    }

    fn render_place(&self, place: &IntermediatePlace, labels: &HashMap<VarId, String>) -> String {
        match place {
            IntermediatePlace::Local(var) => self.render_var(*var, labels),
            IntermediatePlace::Field { parent, field } => {
                format!("field {}, {}", self.render_var(*parent, labels), field)
            }
        }
    }

    fn render_expr(&self, expr: &IntermediateExpr, labels: &HashMap<VarId, String>) -> String {
        match &expr.kind {
            IntermediateExprKind::Bool(value) => value.to_string(),
            IntermediateExprKind::Int(value) => value.to_string(),
            IntermediateExprKind::Float(value) => value.to_string(),
            IntermediateExprKind::StringLiteral(handle) => self.render_string_ref(handle),
            IntermediateExprKind::Struct { id, exprs } => format!(
                "{} {{{}}}",
                self.render_ty_id(*id),
                exprs
                    .iter()
                    .map(|expr| self.render_value_ref(*expr))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IntermediateExprKind::FieldAccess { parent, field } => {
                format!("field {}, {}", self.render_value_ref(*parent), field)
            }
            IntermediateExprKind::Binary { lhs, rhs, operator } => format!(
                "{} {}, {}",
                self.render_operator(*operator),
                self.render_value_ref(*lhs),
                self.render_value_ref(*rhs)
            ),
            IntermediateExprKind::Identifier(var) => self.render_var(*var, labels),
            IntermediateExprKind::Component {
                id,
                props,
                children,
            } => format!(
                "{}(props=[{}], children=[{}])",
                self.render_context_ref(*id),
                props
                    .iter()
                    .map(|prop| {
                        prop.map(|value| self.render_value_ref(value))
                            .unwrap_or_else(|| "default".to_string())
                    })
                    .collect::<Vec<_>>()
                    .join(", "),
                children
                    .iter()
                    .map(|child| self.render_value_ref(*child))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IntermediateExprKind::FunctionCall { id, args } => format!(
                "call {}({})",
                self.render_context_ref(*id),
                args.iter()
                    .map(|arg| self.render_value_ref(*arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IntermediateExprKind::Native(native) => match &native.kind {
                NativeComponentKind::Text { text } => format!(
                    "Text(text={}, props=[{}])",
                    self.render_value_ref(*text),
                    native
                        .props
                        .iter()
                        .map(|prop| {
                            prop.map(|value| self.render_value_ref(value))
                                .unwrap_or_else(|| "default".to_string())
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                NativeComponentKind::Rect { children } => format!(
                    "Rect(children=[{}], props=[{}])",
                    children
                        .iter()
                        .map(|child| self.render_value_ref(*child))
                        .collect::<Vec<_>>()
                        .join(", "),
                    native
                        .props
                        .iter()
                        .map(|prop| {
                            prop.map(|value| self.render_value_ref(value))
                                .unwrap_or_else(|| "default".to_string())
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
            },
        }
    }

    fn render_type(&self, ty: &IntermediateType) -> String {
        match ty {
            IntermediateType::Void => "void".to_string(),
            IntermediateType::Int => "int".to_string(),
            IntermediateType::Float => "float".to_string(),
            IntermediateType::Bool => "bool".to_string(),
            IntermediateType::Component => "Component".to_string(),
            IntermediateType::Str => "%StrHandle".to_string(),
            IntermediateType::Complex(fields) => format!(
                "struct{{{}}}",
                fields
                    .iter()
                    .map(|field| self.render_type(field))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            IntermediateType::Vector(ty) => format!("vec<{}>", self.render_type(ty)),
            IntermediateType::Reference(ty) => self
                .types
                .get(ty)
                .map(|lowered| self.render_ty_id(*lowered))
                .unwrap_or_else(|| format!("ref_ty{}", ty.as_raw())),
            IntermediateType::Function(args, ret) => format!(
                "fn({}) -> {}",
                args.iter()
                    .map(|arg| self.render_type(arg))
                    .collect::<Vec<_>>()
                    .join(", "),
                self.render_type(ret)
            ),
        }
    }

    fn render_ty_id(&self, ty: TyId) -> String {
        self.type_names
            .get(&ty)
            .map(|name| format!("%{name}"))
            .unwrap_or_else(|| format!("%ty{}", ty.as_raw()))
    }

    fn render_value_ref(&self, value: ValueId) -> String {
        format!("%e{}", value.as_raw())
    }

    fn render_var(&self, var: VarId, labels: &HashMap<VarId, String>) -> String {
        labels
            .get(&var)
            .cloned()
            .unwrap_or_else(|| format!("l{}", labels.len()))
    }

    fn render_context_ref(&self, handle: ContextHandle) -> String {
        match &self.contexts[handle.0].ty {
            IntermediateContextType::Function { name, .. } => format!("#{}", name),
            IntermediateContextType::Component { name, .. } => format!("%{}", name),
        }
    }

    fn render_string_ref(&self, handle: &crate::intermediate::string::StringHandle) -> String {
        self.strings
            .handles()
            .iter()
            .position(|candidate| candidate == handle)
            .map(|idx| format!("@str{idx}"))
            .unwrap_or_else(|| "@str?".to_string())
    }

    fn render_operator(&self, operator: Operator) -> &'static str {
        match operator {
            Operator::Add => "add",
            Operator::Sub => "sub",
            Operator::Star => "mul",
            Operator::Slash => "div",
            Operator::Equals => "cmp",
            Operator::GreaterThan => "cmpgt",
            Operator::GreaterThanOrEqual => "cmpgte",
            Operator::LessThan => "cmplt",
            Operator::LessThanOrEqual => "cmplte",
            Operator::LogicAnd => "and",
            Operator::LogicOr => "or",
        }
    }

    fn is_component_type(&self, ty: TyId) -> bool {
        self.context_mapping
            .get(&ty)
            .map(|handle| {
                matches!(
                    self.contexts[handle.0].ty,
                    IntermediateContextType::Component { .. }
                )
            })
            .unwrap_or(false)
    }

    fn variable_labels(&self, context: &IntermediateContext) -> HashMap<VarId, String> {
        let mut labels = HashMap::new();

        if let IntermediateContextType::Function {
            args, instructions, ..
        } = &context.ty
        {
            for (idx, (_, arg)) in args.iter().enumerate() {
                labels.insert(*arg, format!("p{idx}"));
            }

            let mut next_local = 0usize;
            for instruction in instructions {
                match &instruction.kind {
                    IntermediateInstructionKind::Alloc(var)
                    | IntermediateInstructionKind::Read(var) => {
                        if !labels.contains_key(var) {
                            labels.insert(*var, format!("l{next_local}"));
                            next_local += 1;
                        }
                    }
                    IntermediateInstructionKind::Move { target, .. } => match target {
                        IntermediatePlace::Local(var)
                        | IntermediatePlace::Field { parent: var, .. } => {
                            if !labels.contains_key(var) {
                                labels.insert(*var, format!("l{next_local}"));
                                next_local += 1;
                            }
                        }
                    },
                    IntermediateInstructionKind::Ret(_) => {}
                }
            }
        }

        labels
    }
}
