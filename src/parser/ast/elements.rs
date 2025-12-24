use std::borrow::Cow;

use crate::parser::ast::{ASTExpression, GenericIdentifier, MacroElementArgs, Span};

#[derive(Default, Debug, Clone)]
pub enum PropertyModifier {
    ///Property visible to everyone
    Public,
    ///Property visible only to the element itself.
    #[default]
    Private,
    ///Property visible only for the children
    ChildrenPublic,
    ///Property visible only for the parents
    ParentPublic,
}
#[derive(Debug)]
pub struct ElementDeffinition {
    pub kind: ElementDeffinitionKind,
    pub span: Span,
}
#[derive(Debug)]
pub enum ElementDeffinitionKind {
    RawJs(Cow<'static, str>),
    MacroCall {
        name: String,
        args: MacroElementArgs,
    },
    Property {
        name: String,
        modifier: PropertyModifier,
        ty: Option<GenericIdentifier>,
        rhs: Option<ASTExpression>,
    },
    Child(ElementExpression),
}
#[derive(Debug)]
pub enum ElementValue {
    Assign {
        prop_name: String,
        rhs: ASTExpression,
        span: Span,
    },
    Element(ElementExpression),
}

#[derive(Debug)]
pub struct ElementExpression {
    pub name: GenericIdentifier,
    pub values: Vec<ElementValue>,
    pub span: Span,
}