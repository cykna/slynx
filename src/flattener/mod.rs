pub mod element;
pub mod expr;
pub mod types;

use crate::flattener::element::{ElementDef, ElementValueFlattened};
use crate::flattener::expr::{Expression, MaybeExpr};
use crate::flattener::types::{FlattenedInternalType, FlattenedType};
use crate::hir::declaration::{
    ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression, HirExpressionKind,
};
use crate::hir::types::HirType;

#[derive(Debug)]
#[repr(u8)]
pub enum HirNodeType {
    ComponentDeffinition,
    Property,
    Expression,
}

#[derive(Clone, Copy)]
#[repr(C)]
pub struct HirNodeProperty {
    ///Index of the property inside the current component
    pub(crate) index: u32,
    ///The default value of it.
    pub(crate) default_value: MaybeExpr,
}

#[repr(C)]
pub union HirNodeValue {
    pub(crate) property: HirNodeProperty,
    pub(crate) expression: Expression,
    pub(crate) element_def: ElementDef,
}

#[repr(C)]
pub struct HirNode {
    pub(crate) ty: HirNodeType,
    pub(crate) value: HirNodeValue,
}

#[derive(Debug)]
#[repr(C)]
pub struct FlattenedHir {
    ///Array of complex types
    types: *const FlattenedType,
    types_len: usize,
    types_cap: usize,
    nodes: *const HirNode,
    nodes_len: usize,
    nodes_cap: usize,
}

#[repr(C)]
pub struct FlattenedVecHir {
    ///Array of complex types
    types: Vec<FlattenedType>,
    nodes: Vec<HirNode>,
}

impl FlattenedHir {
    pub fn vected(&self) -> FlattenedVecHir {
        unsafe {
            FlattenedVecHir {
                types: Vec::from_raw_parts(self.types as *mut _, self.types_len, self.types_cap),
                nodes: Vec::from_raw_parts(self.nodes as *mut _, self.nodes_len, self.nodes_cap),
            }
        }
    }
}

pub struct Flattener {
    pub(crate) types: Vec<FlattenedType>,
    pub(crate) nodes: Vec<HirNode>,
    ///An array to keep track of expressions that depend on each other
    pub(crate) expressions: Vec<Expression>,
}
impl Flattener {
    pub fn new() -> Self {
        let mut out = Self {
            types: Vec::new(),
            nodes: Vec::new(),
            expressions: Vec::new(),
        };
        out.setup();
        out
    }

    ///OBLIGATORY TO MODIFY THIS WHEN ADDING A NEW PRIMITIVE TYPE. OR ELSE THE INDEXES WILL BE INCORRECT
    pub fn setup(&mut self) {
        self.types.push(FlattenedType::int());
        self.types.push(FlattenedType::float());
        self.types.push(FlattenedType::duplet(false));
        self.types.push(FlattenedType::duplet(true));
        self.types.push(FlattenedType::quadruplet(false));
        self.types.push(FlattenedType::quadruplet(true));
        self.types.push(FlattenedType::generic_component());
    }

    pub fn vector_of(&self, ty: usize) -> FlattenedType {
        FlattenedType {
            kind: types::FlattenedTypeKind::Vector,
            ty: FlattenedInternalType { vector: ty },
        }
    }

    pub fn retrieve_type(&self, ty: &HirType) -> usize {
        match ty {
            HirType::Int => 0,
            HirType::Float => 1,
            HirType::Int16x2 => 2,
            HirType::Uint16x2 => 3,
            HirType::Int8x4 => 4,
            HirType::Uint8x4 => 5,
            HirType::GenericComponent => 6,
            HirType::Vector { ty } => {
                let internal = self.retrieve_type(ty);
                internal | (1 << 63)
            }
            other => unimplemented!("{other:?}"),
        }
    }

    ///Inserts the provided `expr` and returns it's id
    pub fn insert_expr(&mut self, expr: Expression) -> u64 {
        let idx = self.expressions.len();
        self.expressions.push(expr);
        idx as u64
    }

    pub fn flatten_expression(&mut self, expr: HirExpression) -> u64 {
        let doublet_flag = matches!(&expr.kind, HirExpressionKind::Uint8x4(_, _, _, _));
        let quadruplet_flat = matches!(&expr.kind, HirExpressionKind::Uint16x2(_, _,));
        match expr.kind {
            HirExpressionKind::Int(int) => self.insert_expr(Expression::int(int)),

            HirExpressionKind::Float(float) => self.insert_expr(Expression::float(float)),
            HirExpressionKind::Identifier(hir) => self.insert_expr(Expression::identifier(hir.0)),
            HirExpressionKind::Uint8x4(a, b, c, d) | HirExpressionKind::Int8x4(a, b, c, d) => {
                let a = self.flatten_expression(*a);
                let b = self.flatten_expression(*b);
                let c = self.flatten_expression(*c);
                let d = self.flatten_expression(*d);
                self.insert_expr(Expression::quadruplet(a, b, c, d, doublet_flag))
            }
            HirExpressionKind::Int16x2(a, b) | HirExpressionKind::Uint16x2(a, b) => {
                let a = self.flatten_expression(*a);
                let b = self.flatten_expression(*b);
                self.insert_expr(Expression::duplet(a, b, quadruplet_flat))
            }
            HirExpressionKind::Binary { lhs, op, rhs } => {
                let lhs = self.flatten_expression(*lhs);
                let rhs = self.flatten_expression(*rhs);
                self.insert_expr(Expression::binary(lhs, rhs, op))
            }
            HirExpressionKind::Element { name, values } => {
                unimplemented!()
            }
        }
    }
    pub fn flatten_hir(&mut self, hir: Vec<HirDeclaration>) -> FlattenedHir {
        for hir in hir {
            match hir.kind {
                HirDeclarationKind::Function { statments } => {}
                HirDeclarationKind::ElementDeclaration { props } => {
                    let HirType::Component { props: ty } = hir.ty else {
                        unreachable!();
                    };
                    let mut defs = Vec::new();
                    let tys = ty
                        .iter()
                        .map(|v| self.retrieve_type(&v.2))
                        .collect::<Vec<_>>();
                    self.types.push(FlattenedType::complex(tys));
                    for prop in props {
                        match prop {
                            ElementValueDeclaration::Property { index, value, span } => {
                                let val = ElementValueFlattened::property(
                                    if let Some(value) = value {
                                        let expr = self.flatten_expression(value);
                                        MaybeExpr::new(expr)
                                    } else {
                                        MaybeExpr::empty()
                                    },
                                    {
                                        let idx = self.retrieve_type(&ty[index].2);
                                        if idx & (1 << 63) != 0 {
                                            self.types[idx & !(1 << 63)]
                                        } else {
                                            self.types[idx]
                                        }
                                    },
                                );
                                defs.push(val);
                            }
                            ElementValueDeclaration::Child { name, values, span } => {}
                        }
                    }

                    let node = HirNode {
                        ty: HirNodeType::ComponentDeffinition,
                        value: HirNodeValue {
                            element_def: ElementDef {
                                defs: defs.as_ptr(),
                                len: defs.len(),
                            },
                        },
                    };
                    self.nodes.push(node);
                }
            }
        }
        FlattenedHir {
            types_len: self.types.len(),
            types_cap: self.types.capacity(),
            nodes_len: self.nodes.len(),
            nodes_cap: self.nodes.capacity(),
            types: self.types.as_ptr(),
            nodes: self.nodes.as_ptr(),
        }
    }
}
