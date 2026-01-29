use swc_atoms::Atom;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{Expr, IdentName, MemberExpr, MemberProp, ObjectLit, PropOrSpread};

use crate::{
    compiler::{js::WebCompiler, slynx_compiler::SlynxCompiler},
    intermediate::{
        IntermediateRepr,
        context::IntermediateContext,
        id::{ContextHandle, ValueId},
    },
};

impl WebCompiler {
    pub fn compile_struct(
        &mut self,
        exprs: &[ValueId],
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
        handle: ContextHandle,
    ) -> Expr {
        Expr::Object(ObjectLit {
            span: DUMMY_SP,
            props: exprs
                .iter()
                .enumerate()
                .map(|(idx, ptr)| {
                    PropOrSpread::Prop(Box::new(Self::prop(
                        &format!("f{idx}"),
                        self.compile_expression(&ctx.exprs[*ptr], ctx, ir, handle),
                    )))
                })
                .collect(),
        })
    }
    ///Compiles a field expression such as `variable.b`
    pub fn compile_field_access(
        &mut self,
        parent: ValueId,
        field: usize,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
        handle: ContextHandle,
    ) -> Expr {
        let parent = self.compile_expression(&ctx.exprs[parent], ctx, ir, handle);
        Expr::Member(MemberExpr {
            span: DUMMY_SP,
            obj: Box::new(parent),
            prop: MemberProp::Ident(IdentName {
                span: DUMMY_SP,
                sym: Atom::new(format!("f{field}")),
            }),
        })
    }
}
