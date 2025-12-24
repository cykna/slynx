use crate::{hir::HirId, intermediate::string::StringHandle, parser::ast::Operator};

///A Native element that is not user-defined
#[derive(Debug, Clone)]
pub enum NativeElementKind {
    Text {
        ///Pointer to the expression
        text: usize,
    },
    Rect {
        children: Vec<usize>,
    },
}

#[derive(Debug, Clone)]
pub struct NativeElement {
    kind: NativeElementKind,
    props: Vec<Option<usize>>,
}

#[derive(Debug, Clone)]
pub enum IntermediateExpr {
    Int(i32),
    Float(f32),
    StringLiteral(StringHandle),
    Binary {
        ///Inside the IntermediateRepr, the index of the lhs expression
        lhs: usize,
        ///Inside the IntermediateRepr, the index of the rhs expression
        rhs: usize,
        operator: Operator,
    },
    Identifier(HirId),
    ///An element expresssion. The props are the public children that may require some input. A None value will result in passing to them undefined
    ///and a Some(idx) will pass to them the expression on the `idx` of the current context
    ///The children are the children for this element, so, an array of indices for more element expressions inside the ccurrent context
    Element {
        id: HirId,
        props: Vec<Option<usize>>,
        children: Vec<usize>,
    },
    Native(NativeElement),
}

impl IntermediateExpr {
    ///Creates a native `text` element with the provided `text`
    pub fn native_text(text: usize, props: Vec<Option<usize>>) -> Self {
        Self::Native(NativeElement {
            kind: NativeElementKind::Text { text },
            props,
        })
    }
    ///Creates a native `rect` element with the provided `children`
    pub fn native_rect(children: Vec<usize>, props: Vec<Option<usize>>) -> Self {
        Self::Native(NativeElement {
            kind: NativeElementKind::Rect { children },
            props,
        })
    }
}
