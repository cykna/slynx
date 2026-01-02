use swc_common::{DUMMY_SP, SyntaxContext};
use swc_ecma_ast::{
    AssignPat, BindingIdent, BlockStmt, Decl, EmptyStmt, FnDecl, Function, Param, Pat, Stmt,
};

use crate::{
    compiler::{js::WebCompiler, slynx_compiler::SlynxCompiler},
    hir::HirId,
    intermediate::{
        IntermediateRepr,
        context::{IntermediateContext, IntermediateProperty},
    },
};

impl WebCompiler {
    pub fn retrieve_component_params(
        &mut self,
        properties: &[IntermediateProperty],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> Vec<Param> {
        properties
            .iter()
            .enumerate()
            .map(|(idx, prop)| {
                let ident = Pat::Ident(BindingIdent {
                    id: self.map_name(prop.id, &format!("a{idx}")),
                    type_ann: None,
                });
                Param {
                    pat: if let Some(default) = prop.default_value {
                        Pat::Assign(AssignPat {
                            span: DUMMY_SP,
                            left: Box::new(ident),
                            right: Box::new(self.compile_expression(&ctx.exprs[default], ctx, ir)),
                        })
                    } else {
                        ident
                    },
                    span: DUMMY_SP,
                    decorators: Vec::new(),
                }
            })
            .collect()
    }

    pub fn compile_component_body(&mut self) -> BlockStmt {
        BlockStmt {
            stmts: vec![Stmt::Empty(EmptyStmt { span: DUMMY_SP })],
            span: DUMMY_SP,
            ctxt: SyntaxContext::empty(),
        }
    }

    pub fn compile_component(
        &mut self,
        id: HirId,
        properties: &[IntermediateProperty],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) {
        let params = self.retrieve_component_params(properties, ctx, ir);
        let func = Function {
            params,
            is_async: false,
            is_generator: false,
            decorators: Vec::new(),
            span: DUMMY_SP,
            ctxt: SyntaxContext::empty(),
            type_params: None,
            return_type: None,
            body: Some(self.compile_component_body()),
        };
        let f = Decl::Fn(FnDecl {
            ident: self.retrieve_next_component_name(id),
            declare: true,
            function: Box::new(func),
        });
        self.script.body.push(Stmt::Decl(f));
    }
}
