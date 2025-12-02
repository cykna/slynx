use std::collections::HashMap;

use crate::{
    hir::HirId,
    intermediate::{
        IntermediateRepr,
        context::{IntermediateContext, IntermediateContextType, IntermediateProperty},
        expr::IntermediateExpr,
        node::IntermediateInstruction,
    },
    parser::ast::Operator,
};

pub struct Compiler {
    indentation_level: usize,
    component_len: u32,
    ///The name of each component, to then, reuse the function names
    names: HashMap<HirId, String>,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            component_len: 0,
            indentation_level: 0,
            names: HashMap::new(),
        }
    }

    #[inline]
    fn get_param(index: usize) -> String {
        format!("param{index}")
    }

    fn get_prop_name(index: u32) -> String {
        format!("prop{index}")
    }

    fn get_child_name(index: u32) -> String {
        format!("child{index}")
    }

    ///Generates the syntax for the function creation for the next component and registers it on the names map
    fn next_component(
        &mut self,
        ctx: &IntermediateContext,
        props: &[IntermediateProperty],
        id: HirId,
        ir: &IntermediateRepr,
    ) -> String {
        let component_name = self.next_component_name();
        let mut func = format!("function {}(", component_name);
        self.names.insert(id, component_name);
        for i in 0..props.len() {
            func.push_str(&Self::get_param(i));
            if let Some(default) = props[i].default_value {
                let default = self.compile_expression(&ctx.exprs[default], ctx, ir);
                func.push_str(&format!(" = {}", default));
            }
            func.push(',');
        }
        func.pop();
        func.push_str("){\n");
        func
    }

    ///Generates the next name for a component
    fn next_component_name(&mut self) -> String {
        let out = format!("component{}", self.component_len);
        self.component_len += 1;
        out
    }

    fn increase_indentation(&mut self) {
        self.indentation_level += 1;
    }
    fn decrease_indentation(&mut self) {
        self.indentation_level -= 1;
    }
    fn indented_string(&self, str: &str) -> String {
        let mut out = "  ".repeat(self.indentation_level);
        out.push_str(str);
        out
    }

    fn compile_expression(
        &mut self,
        expr: &IntermediateExpr,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> String {
        match expr {
            IntermediateExpr::StringLiteral(handle) => format!("\"{}\"", &ir.strings[handle]),
            IntermediateExpr::Int(int) => int.to_string(),
            IntermediateExpr::Float(float) => float.to_string(),

            IntermediateExpr::Binary { lhs, rhs, operator } => {
                let lhs = self.compile_expression(&ctx.exprs[*lhs as usize], ctx, ir);
                let rhs = self.compile_expression(&ctx.exprs[*rhs as usize], ctx, ir);
                format!(
                    "{lhs}{}{rhs}",
                    match operator {
                        Operator::Add => '+',
                        Operator::Sub => '-',
                        Operator::Star => '*',
                        Operator::Slash => '/',
                    }
                )
            }
            IntermediateExpr::Identifier(expr) => {
                let name = self.names.get(&expr).unwrap();
                name.to_string()
            }
            IntermediateExpr::Element {
                id,
                props,
                children,
            } => {
                let mut value = self.names.get(id).unwrap().clone();
                value.push('(');
                for prop in props {
                    if let Some(expr_idx) = prop {
                        value.push_str(&self.compile_expression(&ctx.exprs[*expr_idx], ctx, ir));
                    } else {
                        value.push_str("undefined");
                    }
                    value.push(',');
                }

                if children.len() > 0 {
                    value.push('[');
                    for child in children {
                        value.push_str(&self.compile_expression(&ctx.exprs[*child], ctx, ir));
                        value.push(',');
                    }
                    value.pop();
                    value.push_str("])");
                } else {
                    value.push(')');
                }

                value
            }
        }
    }

    ///Compiles the given current property with the given `index`. Its default value is defined at `prop`, if it exists, else, it's value will be
    ///the param itself of the function
    fn compile_property(
        &mut self,
        prop: &IntermediateProperty,
        index: u32,
        _: &IntermediateRepr,
        _: &IntermediateContext,
    ) -> String {
        let prop_value = Self::get_prop_name(index);
        self.names.insert(prop.id, prop_value.clone());
        let param_name = Self::get_param(index as usize);

        format!("{prop_value}:{param_name},")
    }

    ///Compiles the given `child_expr_index`, asserting that, the expression on the provided parameter, is the index for an element.
    ///The `index` is the index of the child itself insteadof it's expression
    ///expression on the given `ctx`. By now the `ir` is being unused
    fn compile_child(
        &mut self,
        child_expr_index: usize,
        index: u32,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> String {
        let IntermediateExpr::Element { id, props, .. } = &ctx.exprs[child_expr_index] else {
            unreachable!("Index should map to a element expression");
        };
        let child = Self::get_child_name(index);
        let params = {
            let mut params = String::new();
            for prop in props {
                if let Some(prop) = prop {
                    let mut content = self.compile_expression(&ctx.exprs[*prop], ctx, ir);
                    content.push(',');
                    params.push_str(&content);
                } else {
                    params.push_str("undefined,");
                }
            }
            params.pop();
            params
        };
        let fname = self.names.get(id).unwrap();
        format!("out.{child} = {fname}({params})")
    }

    fn compile_component(&mut self, ctx: &IntermediateContext, ir: &IntermediateRepr) -> String {
        let IntermediateContextType::Element {
            properties,
            children,
            js,
        } = &ctx.ty
        else {
            unreachable!();
        };
        let next = self.next_component(ctx, properties, ctx.id, ir);
        let mut out = self.indented_string(&next);
        self.increase_indentation();
        {
            let indented = self.indented_string("const out = {");
            out.push_str(&indented);
        }
        for (index, prop) in properties.iter().enumerate() {
            let compiled = self.compile_property(prop, index as u32, ir, ctx);
            out.push_str(&compiled);
        }

        out.pop();
        out.push_str("}\n");

        for js in js.iter() {
            out.push_str(&self.indented_string(&js));
            out.push('\n');
        }
        for (idx, child) in children.iter().enumerate() {
            let child = self.compile_child(*child, idx as u32, ctx, ir);
            let child = self.indented_string(&child);
            out.push_str(&child);
            out.push('\n');
        }
        out.push_str(&self.indented_string("return out;"));
        self.decrease_indentation();

        out.push_str("\n}\n");

        out
    }

    fn compile_instruction(
        &mut self,
        instruction: &IntermediateInstruction,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> String {
        match instruction {
            IntermediateInstruction::Alloc => unimplemented!("Allocs not implemented"),
            IntermediateInstruction::Move { .. } => {
                unimplemented!("Moves not implemented")
            }
            IntermediateInstruction::Read(idx) => {
                self.compile_expression(&ctx.exprs[*idx], ctx, ir)
            }
            IntermediateInstruction::Ret(idx) => {
                format!(
                    "return {}",
                    self.compile_expression(&ctx.exprs[*idx], ctx, ir)
                )
            }
            IntermediateInstruction::Js(js) => js.to_string(),
        }
    }

    fn compile_function(
        &mut self,
        param_len: usize,
        name: &str,
        instructions: &[IntermediateInstruction],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> String {
        let args = {
            let mut out = String::new();
            for i in 0..param_len as u32 {
                out.push_str("param");
                out.push(unsafe { char::from_u32_unchecked(i) });
                out.push(',');
            }
            out.pop();
            out
        };
        let mut out = format!("function {name}({args}){{\n");
        self.increase_indentation();
        for instruction in instructions {
            let instruction = self.compile_instruction(instruction, ctx, ir);
            out.push_str(&self.indented_string(&instruction));
            out.push('\n');
        }
        out.push('}');
        if name == "main" {
            out.push_str("\nmain();");
        }
        self.decrease_indentation();
        out
    }

    ///The flattener has everything it's required
    pub fn compile(&mut self, ir: &IntermediateRepr) -> String {
        let mut out = String::new();
        for ctx in ir.contexts.iter() {
            let content = match &ctx.ty {
                IntermediateContextType::Function {
                    name,
                    instructions,
                    param_len,
                } => self.compile_function(*param_len, name, instructions, ctx, ir),
                IntermediateContextType::Element { .. } => self.compile_component(ctx, ir),
            };
            out.push_str(&content);
        }
        out
    }
}
