use common::{Span, SymbolPointer};

use crate::hir::{
    DeclarationId, PropertyId, TypeId, VariableId,
    model::{HirExpression, HirStatement},
};

#[derive(Debug)]
#[repr(C)]
pub enum SpecializedComponent {
    Text {
        text: Box<HirExpression>,
    },
    Div {
        children: Vec<ComponentMemberDeclaration>,
    },
}

impl SpecializedComponent {
    ///Creates a new specialized Text component with the given `text`
    pub fn new_text(text: HirExpression) -> Self {
        Self::Text {
            text: Box::new(text),
        }
    }
    ///Creates a new specialized Div component with the given `children`
    pub fn new_div(children: Vec<ComponentMemberDeclaration>) -> Self {
        Self::Div { children }
    }
}

#[derive(Debug)]
#[repr(C)]
pub struct HirDeclaration {
    pub kind: HirDeclarationKind,
    pub id: DeclarationId, // Changed from HirId
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug)]
#[repr(C)]
pub enum HirDeclarationKind {
    Object,
    Function {
        statements: Vec<HirStatement>,
        args: Vec<VariableId>, // Changed from HirId - function arguments are variables
        name: SymbolPointer,
    },
    ComponentDeclaration {
        name: SymbolPointer,
        props: Vec<ComponentMemberDeclaration>,
    },
    Alias,
}

#[derive(Debug)]
#[repr(C)]
pub enum ComponentMemberDeclaration {
    Property {
        id: PropertyId, // Changed from HirId
        /// The index of the property on the component
        index: usize,
        value: Option<HirExpression>,
        span: Span,
    },
    Child {
        name: TypeId, // Still HirId - reference to a component declaration
        values: Vec<ComponentMemberDeclaration>,
        span: Span,
    },
    Specialized(SpecializedComponent),
}

impl ComponentMemberDeclaration {
    pub fn new_property(index: usize, value: Option<HirExpression>, span: Span) -> Self {
        Self::Property {
            id: PropertyId::new(),
            index,
            value,
            span,
        }
    }
    pub fn new_child(name: TypeId, values: Vec<ComponentMemberDeclaration>, span: Span) -> Self {
        Self::Child { name, values, span }
    }
}

impl HirDeclaration {
    pub fn new_function(
        statements: Vec<HirStatement>,
        args: Vec<VariableId>,
        name: SymbolPointer,
        span: Span,
        id: DeclarationId,
        ty: TypeId,
    ) -> Self {
        Self {
            kind: HirDeclarationKind::Function {
                statements,
                args,
                name,
            },
            span,
            id,
            ty,
        }
    }
    pub fn new_object(decl: DeclarationId, declty: TypeId, span: Span) -> Self {
        Self {
            kind: HirDeclarationKind::Object,
            id: decl,
            ty: declty,
            span,
        }
    }
    pub fn new_alias(decl: DeclarationId, ty: TypeId, span: Span) -> Self {
        Self {
            id: decl,
            kind: HirDeclarationKind::Alias,
            ty,
            span,
        }
    }
}
