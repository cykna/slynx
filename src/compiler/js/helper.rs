use swc_atoms::Wtf8Atom;
use swc_common::DUMMY_SP;
use swc_ecma_ast::{Expr, KeyValueProp, Prop};

use crate::compiler::js::WebCompiler;

impl WebCompiler {
    ///Creates an 'undefined' expression
    pub fn undefined() -> Box<Expr> {
        Expr::undefined(DUMMY_SP)
    }

    ///Creates a new property with provided `name` and `value`
    pub fn prop(name: &str, value: Expr) -> Prop {
        Prop::KeyValue(KeyValueProp {
            key: swc_ecma_ast::PropName::Str(swc_ecma_ast::Str {
                span: DUMMY_SP,
                value: Wtf8Atom::new(name),
                raw: None,
            }),
            value: value.into(),
        })
    }
}
