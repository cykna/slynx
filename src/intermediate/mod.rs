pub mod context;
pub mod expr;
pub mod node;
pub mod types;
use std::collections::HashMap;

use crate::{
    hir::{
        HirId,
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind,
        },
        types::HirType,
    },
    intermediate::{
        context::{IntermediateContext, IntermediateContextType, IntermediateProperty},
        expr::IntermediateExpr,
        types::IntermediateType,
    },
};

///Struct used to represent the intermediate representation of Slynx. WIll be used when being compiled. This contains the monomorfization of types
///and flat values of everything in the source code. It can be understood as the context itself of the IR
pub struct IntermediateRepr {
    pub contexts: Vec<IntermediateContext>,
    types: Vec<IntermediateType>,
    ///Vector of indices of types Vec<T> and T. This is used to find a vec<T> by T, if it doesn't exist, then creates a new one on types and here
    ///In case, this is (VECTOR_INDEX, TYPE_INDEX). So something like Vec<Vec<Uint16x2>> is, for example (5,4) (4,3), 3 == Uint16x2, 4 == Vec<Uint16x2>, 5 == Vec<4> == Vec<Vec<UInt16x2>>
    pub vecs: Vec<(usize, usize)>,
    pub func_mapping: HashMap<HirId, usize>,
    pub component_mapping: HashMap<HirId, usize>,
    pub types_mapping: HashMap<HirId, usize>,
}

impl IntermediateRepr {
    pub fn new() -> Self {
        Self {
            contexts: Vec::new(),
            types_mapping: HashMap::new(),
            func_mapping: HashMap::new(),
            component_mapping: HashMap::new(),
            types: vec![
                IntermediateType::Void,
                IntermediateType::Int,
                IntermediateType::Float,
                IntermediateType::UWords,
                IntermediateType::Words,
                IntermediateType::UBytes,
                IntermediateType::Bytes,
                IntermediateType::Component,
            ],
            vecs: Vec::new(),
        }
    }

    fn active_context(&mut self) -> &mut IntermediateContext {
        let len = self.contexts.len();
        &mut self.contexts[len - 1]
    }

    ///Creates a new vector with the monomorfized `ty` and returns it's new index
    fn create_vec_type(&mut self, ty: usize) -> usize {
        let idx = self.types.len();
        self.types.push(IntermediateType::Vector(ty));
        idx
    }

    ///Based on the monomorfized type. Returns the index of the vector that its got. For example,
    ///Vec<Uint16x2>. This will search for some vector with type `Uint16x2`. If it doesn't find returns None, else, it will return its index(id)
    fn retrieve_vec_index(&self, monomorfized_type: usize) -> Option<usize> {
        for vec in self.vecs.iter() {
            if vec.1 == monomorfized_type {
                return Some(vec.0);
            }
        }
        None
    }

    fn get_type_from_id(&self, id: &HirId) -> usize {
        *self.types_mapping.get(id).unwrap()
    }

    ///Returns the
    fn get_type_id(&mut self, ty: &HirType) -> usize {
        match ty {
            HirType::Void => 0,
            HirType::Int => 1,
            HirType::Float => 2,
            HirType::Uint16x2 => 3,
            HirType::Int16x2 => 4,
            HirType::Uint8x4 => 5,
            HirType::Int8x4 => 6,
            HirType::GenericComponent | HirType::Component { .. } => 7,
            HirType::Reference { rf, .. } => {
                let index = self
                    .types_mapping
                    .get(&rf)
                    .expect("Type reference not defined");
                *index
            }
            HirType::Vector { ty } => {
                let ty = self.get_type_id(&ty);

                if let Some(v) = self.retrieve_vec_index(ty) {
                    v
                } else {
                    self.create_vec_type(ty)
                }
            }
            HirType::Function { args, return_type } => {
                let _ = args
                    .iter()
                    .map(|arg| self.get_type_id(arg))
                    .collect::<Vec<_>>();
                let _ = self.get_type_id(&return_type);
                unimplemented!("Not implemented for functions")
            }
            HirType::Infer => unreachable!("All infer types should be known during compile time"),
        }
    }

    ///Generates a new expression and returns it's id on the current context
    fn generate_expr(&mut self, expr: HirExpression) -> usize {
        match expr.kind {
            HirExpressionKind::Int(int) => self
                .active_context()
                .insert_expr(IntermediateExpr::Int(int)),
            HirExpressionKind::Float(float) => self
                .active_context()
                .insert_expr(IntermediateExpr::Float(float)),
            HirExpressionKind::Int8x4(a, b, c, d) | HirExpressionKind::Uint8x4(a, b, c, d) => {
                let _ = self.generate_expr(*a);
                let _ = self.generate_expr(*b);
                let _ = self.generate_expr(*c);
                let _ = self.generate_expr(*d);

                unimplemented!("Not implemented shifts")
            }
            HirExpressionKind::Int16x2(a, b) | HirExpressionKind::Uint16x2(a, b) => {
                let _ = self.generate_expr(*a);
                let _ = self.generate_expr(*b);
                unimplemented!("Not implemented shifts")
            }
            HirExpressionKind::Binary { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs);
                let rhs = self.generate_expr(*rhs);
                self.active_context().insert_expr(IntermediateExpr::Binary {
                    lhs,
                    rhs,
                    operator: op,
                })
            }
            HirExpressionKind::Identifier(name) => self
                .active_context()
                .insert_expr(IntermediateExpr::Identifier(name)),
            HirExpressionKind::Element { name, values, .. } => {
                let eidx = self.generate_child(name, values);
                if matches!(
                    self.active_context().ty,
                    IntermediateContextType::Element { .. }
                ) {
                    self.active_context().insert_child(eidx); //child index
                };
                eidx
            }
        }
    }
    ///Creates a new child on the current context and returns the element expression and the child id
    fn generate_child(&mut self, name: HirId, values: Vec<ElementValueDeclaration>) -> usize {
        let mut props = Vec::new();
        let mut children = Vec::new();
        for value in values {
            match value {
                ElementValueDeclaration::Property { index, value, .. } => {
                    let out = if let Some(value) = value {
                        Some(self.generate_expr(value))
                    } else {
                        None
                    };
                    for _ in 0..index - props.len() {
                        props.push(None);
                    }
                    props.push(out);
                }
                ElementValueDeclaration::Child { name, values, .. } => {
                    let idx = self.generate_child(name, values);
                    children.push(idx);
                }
            }
        }
        //expr index
        let eidx = self
            .active_context()
            .insert_expr(IntermediateExpr::Element {
                id: name,
                props,
                children,
            });

        eidx
    }

    ///Creates a new element with the provided informations. Returns the component id and it's type id
    fn generate_element(&mut self, id: HirId, props: Vec<ElementValueDeclaration>) -> usize {
        let cidx = self.contexts.len();
        self.component_mapping.insert(id, cidx);

        let expr = IntermediateContext::new_element(id);
        self.contexts.push(expr);
        for prop in props {
            match prop {
                ElementValueDeclaration::Property { value, id, .. } => {
                    let default_value = if let Some(value) = value {
                        Some(self.generate_expr(value))
                    } else {
                        None
                    };
                    self.active_context()
                        .insert_property(IntermediateProperty { id, default_value });
                }
                ElementValueDeclaration::Child { name, values, .. } => {
                    self.generate_child(name, values);
                }
            }
        }
        cidx
    }

    ///Creates a new function with the provided `name` `id` and `statments` and returns its id
    pub fn generate_function(
        &mut self,
        args_len: usize,
        statments: Vec<HirStatment>,
        name: String,
        id: HirId,
    ) -> usize {
        let cidx = self.contexts.len();
        let ctx = IntermediateContext::new_function(id, name, args_len);
        self.contexts.push(ctx);
        for statment in statments {
            match statment.kind {
                HirStatmentKind::Expression { expr } => {
                    self.generate_expr(expr);
                }
                HirStatmentKind::Return { expr } => {
                    let id = self.generate_expr(expr);
                    self.active_context()
                        .insert_node(node::IntermediateInstruction::Ret(id));
                }
            };
        }
        cidx
    }

    pub fn generate(&mut self, decls: Vec<HirDeclaration>) {
        for decl in decls {
            match decl.kind {
                HirDeclarationKind::Function { statments, name } => {
                    let HirType::Function { args, return_type } = &decl.ty else {
                        unreachable!("Type of function decl should be function")
                    };
                    let args_len = args.len();
                    let args = args.iter().map(|arg| self.get_type_id(arg)).collect();
                    let ret = self.get_type_id(return_type);
                    self.types.push(IntermediateType::Function(args, ret));
                    self.generate_function(args_len, statments, name, decl.id);
                }
                HirDeclarationKind::ElementDeclaration { props } => {
                    let mut tys: Vec<usize> = Vec::new();
                    let HirType::Component { props: ref decl_ty } = decl.ty else {
                        unreachable!("Type of element decl should be component");
                    };
                    for (_, _, ty) in decl_ty {
                        let typ = self.get_type_id(ty);
                        tys.push(typ);
                    }

                    self.generate_element(decl.id, props);
                    self.types.push(IntermediateType::Complex(tys));
                }
            }
        }
    }
}
