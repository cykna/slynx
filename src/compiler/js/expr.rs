use swc_atoms::Atom;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{Expr, MemberExpr, MemberProp, ObjectLit, PropOrSpread, IdentName};

use crate::{
    compiler::{js::{WebCompiler}, slynx_compiler::SlynxCompiler},
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
    ///Compiles a field expression such as `variable.b`
    pub fn compile_field_access(&mut self, parent: usize, field: usize, ctx:&IntermediateContext, ir:&IntermediateRepr) -> Expr {
        let parent = self.compile_expression(&ctx.exprs[parent], ctx, ir);
        Expr::Member(MemberExpr {
            span: DUMMY_SP,
            obj: Box::new(parent),
            prop: MemberProp::Ident(IdentName {
                span: DUMMY_SP,
                sym:Atom::new(format!("f{field}"))
            })
        })
    }
}
