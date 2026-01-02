use swc_common::DUMMY_SP;
use swc_ecma_ast::{Expr, ObjectLit, PropOrSpread};

use crate::{
    compiler::{js::WebCompiler, slynx_compiler::SlynxCompiler},
    hir::HirId,
    intermediate::{IntermediateRepr, context::IntermediateContext},
};

impl WebCompiler {
    pub fn compile_struct(
        &mut self,
        _: &HirId,
        exprs: &[usize],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> Expr {
        Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props: exprs
                .iter()
                .map(|idx| {
                    PropOrSpread::Prop(Box::new(Self::prop(
                        &format!("f{idx}"),
                        self.compile_expression(&ctx.exprs[*idx], ctx, ir),
                    )))
                })
                .collect(),
        })
    }
}
