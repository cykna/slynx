mod contexts;
mod expr;
mod functions;
mod types;

use std::{collections::HashMap, rc::Rc};

use swc_atoms::Atom;
use swc_common::{DUMMY_SP, SourceMap};
use swc_ecma_ast::{Expr, Ident, Lit, Program, Script};
use swc_ecma_codegen::{Config, Emitter, text_writer::JsWriter};

use crate::{
    compiler::slynx_compiler::SlynxCompiler,
    hir::HirId,
    intermediate::{
        IntermediateRepr, context::IntermediateContext, expr::IntermediateExpr,
        node::IntermediateInstruction,
    },
};

#[derive(Debug, Default)]
pub struct WebCompiler {
    script: Script,
    names: HashMap<HirId, Ident>,
    next_component_index: usize,
}

pub fn create_ident(s: &str) -> Ident {
    Ident::new_no_ctxt(Atom::new(s), DUMMY_SP)
}

impl WebCompiler {
    pub fn new() -> Self {
        Self {
            names: HashMap::new(),
            script: Script::default(),
            next_component_index: 0,
        }
    }
    ///Creates the new component name, binds it to be the name of the provided `id` and returns it
    pub fn retrieve_next_component_name(&mut self, id: HirId) -> Ident {
        let name = format!("c{}", self.next_component_index);
        self.next_component_index += 1;
        self.names.insert(id, create_ident(&name));
        self.names.get(&id).cloned().unwrap()
    }

    ///Maps the provided `id` to the provided `name` and returns it's indent
    pub fn map_name(&mut self, id: HirId, name: &str) -> Ident {
        self.names.insert(id, create_ident(name));
        self.names.get(&id).cloned().unwrap()
    }
}

impl SlynxCompiler for WebCompiler {
    type ExpressionType = Expr;
    fn compile_instructions(
        &mut self,
        instructions: &[IntermediateInstruction],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) {
        for inst in instructions {
            match inst {
                IntermediateInstruction::Ret(id) => {
                    self.compile_expression(&ctx.exprs[*id], ctx, ir);
                }
                _ => unimplemented!(),
            }
        }
    }
    fn compile_expression(
        &mut self,
        expr: &IntermediateExpr,
        _: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> Self::ExpressionType {
        match expr {
            IntermediateExpr::StringLiteral(s) => Expr::Lit(Lit::Str(ir.strings[s].into())),
            un => unimplemented!("{un:?}"),
        }
    }

    ///The flattener has everything it's required
    fn compile(mut self, ir: IntermediateRepr) -> Vec<u8> {
        for ctx in ir.contexts.iter() {
            self.hoist_ctx(ctx);
        }
        for ctx in &ir.contexts {
            self.compile_ctx(ctx, &ir);
        }
        {
            let mut buf = Vec::new();
            let cm = Rc::new(SourceMap::default());
            let mut writer = JsWriter::new(cm.clone(), "\n", &mut buf, None);
            writer.set_indent_str("  ");
            let mut emitter = Emitter {
                cfg: Config::default(),
                cm: cm.clone(),
                comments: None,
                wr: writer,
            };
            emitter.emit_program(&Program::Script(self.script)).unwrap();
            String::from_utf8(buf).unwrap().into_bytes()
        }
    }
}
