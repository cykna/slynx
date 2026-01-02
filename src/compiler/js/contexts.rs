use crate::{
    compiler::js::WebCompiler,
    intermediate::{
        IntermediateRepr,
        context::{IntermediateContext, IntermediateContextType},
    },
};

impl WebCompiler {
    pub fn hoist_ctx(&mut self, ctx: &IntermediateContext) {
        match ctx.ty {
            IntermediateContextType::Component { .. } => {}
            IntermediateContextType::Function { .. } => {}
        }
    }

    pub fn compile_ctx(&mut self, ctx: &IntermediateContext, ir: &IntermediateRepr) {
        match &ctx.ty {
            IntermediateContextType::Component { properties, .. } => {
                self.compile_component(ctx.id, &properties, ctx, ir);
            }
            IntermediateContextType::Function { .. } => {}
        }
    }
}
