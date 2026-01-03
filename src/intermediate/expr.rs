use crate::{hir::HirId, intermediate::string::StringHandle, parser::ast::Operator};

///A Native component that is not user-defined
#[derive(Debug, Clone)]
pub enum NativeComponentKind {
    Text {
        ///Pointer to the expression
        text: usize,
    },
    Rect {
        children: Vec<usize>,
    },
}

#[derive(Debug, Clone)]

pub struct NativeComponent {
    pub kind: NativeComponentKind,
    pub props: Vec<Option<usize>>,
}

#[derive(Debug, Clone)]
pub enum IntermediateExpr {
    Int(i32),
    Float(f32),
    StringLiteral(StringHandle),
    Struct {
        id: HirId,
        exprs: Vec<usize>,
    },
    FieldAccess{
        parent: usize, field:usize
    },
    Binary {
        ///Inside the IntermediateRepr, the index of the lhs expression
        lhs: usize,
        ///Inside the IntermediateRepr, the index of the rhs expression
        rhs: usize,
        operator: Operator,
    },
    Identifier(HirId),
    ///An component expresssion. The props are the public children that may require some input. A None value will result in passing to them undefined
    ///and a Some(idx) will pass to them the expression on the `idx` of the current context
    ///The children are the children for this component, so, an array of indices for more component expressions inside the ccurrent context
    Component {
        id: HirId,
        props: Vec<Option<usize>>,
        children: Vec<usize>,
    },
    Native(NativeComponent),
    
}

impl IntermediateExpr {
    ///Creates a native `text` component with the provided `text`
    pub fn native_text(text: usize, props: Vec<Option<usize>>) -> Self {
        Self::Native(NativeComponent {
            kind: NativeComponentKind::Text { text },
            props,
        })
    }
    ///Creates a native `rect` component with the provided `children`
    pub fn native_rect(children: Vec<usize>, props: Vec<Option<usize>>) -> Self {
        Self::Native(NativeComponent {
            kind: NativeComponentKind::Rect { children },
            props,
        })
    }
}
