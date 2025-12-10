use crate::{hir::HirId, intermediate::string::StringHandle, parser::ast::Operator};

#[derive(Debug, Clone)]
pub enum IntermediateExpr {
    Int(i32),
    Float(f32),
    StringLiteral(StringHandle),

    IntAdd(usize, usize),
    IntSub(usize, usize),
    IntMul(usize, usize),
    IntDiv(usize, usize),
    IntMod(usize, usize),
    IntNeg(usize),
    IntRsh(usize, usize),
    IntLsh(usize, usize),
    IntAnd(usize, usize),
    IntOr(usize, usize),
    IntXor(usize, usize),
    IntNot(usize),

    FloatAdd(usize, usize),
    FloatSub(usize, usize),
    FloatMul(usize, usize),
    FloatDiv(usize, usize),
    FloatMod(usize, usize),
    FloatNeg(usize),

    CmpEq(usize, usize),
    CmpNe(usize, usize),
    CmpLt(usize, usize),
    CmpLe(usize, usize),
    CmpGt(usize, usize),
    CmpGe(usize, usize),

    Read(HirId),
    ///An element expresssion. The props are the public children that may require some input. A None value will result in passing to them undefined
    ///and a Some(idx) will pass to them the expression on the `idx` of the current context
    ///The children are the children for this element, so, an array of indices for more element expressions inside the ccurrent context
    Element {
        id: HirId,
        props: Vec<Option<usize>>,
        children: Vec<usize>,
    },
}
