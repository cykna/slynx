use crate::intermediate::{
    IntermediateRepr, context::IntermediateContext, expr::IntermediateExpr,
    node::IntermediateInstruction,
};

///A trait to define how the Slynx IR will be compiled
pub trait SlynxCompiler {
    type ExpressionType;
    fn compile_instructions(
        &mut self,
        instructions: &Vec<IntermediateInstruction>,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    );
    fn compile_expression(
        &mut self,
        expr: &IntermediateExpr,
        ctx: &IntermediateContext,
        ir: &IntermediateRepr,
    ) -> Self::ExpressionType;
    fn compile(self, ir: IntermediateRepr) -> Vec<u8>;
}
