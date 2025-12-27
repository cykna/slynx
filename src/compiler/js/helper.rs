use swc_common::DUMMY_SP;
use swc_ecma_ast::Expr;

use crate::compiler::js::WebCompiler;

impl WebCompiler {
    ///Creates an 'undefined' expression
    pub fn undefined() -> Box<Expr> {
        Expr::undefined(DUMMY_SP)
    }
}
