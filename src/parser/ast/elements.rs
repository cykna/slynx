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
///A member on a component, this can be a property or a child expression
pub struct ComponentMember {
    pub kind: ComponentMemberKind,
    pub span: Span,
}
#[derive(Debug)]
pub enum ComponentMemberKind {
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
    Child(ComponentExpression),
}
#[derive(Debug)]
pub enum ComponentMemberValue {
    Assign {
        prop_name: String,
        rhs: ASTExpression,
        span: Span,
    },
    Child(ComponentExpression),
}

#[derive(Debug)]
pub struct ComponentExpression {
    pub name: GenericIdentifier,
    pub values: Vec<ComponentMemberValue>,
    pub span: Span,
}