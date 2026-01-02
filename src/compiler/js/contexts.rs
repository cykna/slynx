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
            IntermediateContextType::Component { .. } => {
                self.retrieve_next_component_name(ctx.id);
            }
            IntermediateContextType::Function { .. } => {
                self.retrieve_next_func_name(ctx.id);
            }
        }
    }

    pub fn compile_ctx(&mut self, ctx: &IntermediateContext, ir: &IntermediateRepr) {
        match &ctx.ty {
            IntermediateContextType::Component { properties, .. } => {
                self.compile_component(properties, ctx, ir);
            }
            IntermediateContextType::Function { .. } => self.compile_function(ctx, ir),
        }
    }
}
