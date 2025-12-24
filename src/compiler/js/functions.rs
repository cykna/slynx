use swc_ecma_ast::{Decl, FnDecl, Function, Param, Pat};

use crate::{compiler::{js::WebCompiler, slynx_compiler::SlynxCompiler}, hir::HirId, intermediate::context::IntermediateProperty};

impl WebCompiler {
    pub fn compile_component(&mut self, id: HirId, properties:&Vec<IntermediateProperty>) {
        let func = Function {
            params: properties.iter().map(|prop| {
                let pat = if let Some(default) = prop.default_value {
                    let rhs = self.compile_expression(expr, ctx);
                }else {
                    
                }
            })
        }
        let f = Decl::Fn(FnDecl {
            ident:self.retrieve_next_component_name(id),
            declare: true,
            function: Box::new()
        })
    }
}