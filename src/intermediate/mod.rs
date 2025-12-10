pub mod context;
pub mod expr;
mod implementation;
pub mod monomorphizer;
pub mod node;
pub mod string;
pub mod types;
use std::collections::HashMap;

use crate::{
    hir::{
        HirId, SlynxHir,
        declaration::{
            ElementValueDeclaration, HirDeclarationKind, HirExpression, HirExpressionKind,
            HirStatment, HirStatmentKind,
        },
        types::HirType,
    },
    intermediate::{
        context::{IntermediateContext, IntermediateContextType, IntermediateProperty},
        expr::IntermediateExpr,
        monomorphizer::Monomorphizer,
        string::StringPool,
        types::IntermediateType,
    },
    parser::ast::Operator,
};

///Struct used to represent the intermediate representation of Slynx. WIll be used when being compiled. This contains the monomorfization of types
///and flat values of everything in the source code. It can be understood as the context itself of the IR
pub struct IntermediateRepr {
    pub contexts: Vec<IntermediateContext>,
    component_mapping: HashMap<HirId, usize>,
    types: HashMap<HirId, IntermediateType>,
    pub strings: StringPool,
}

impl IntermediateRepr {
    pub fn new() -> Self {
        Self {
            contexts: Vec::new(),
            types: HashMap::new(),
            component_mapping: HashMap::new(),
            strings: StringPool::new(),
        }
    }

    fn active_context(&mut self) -> &mut IntermediateContext {
        let len = self.contexts.len();
        &mut self.contexts[len - 1]
    }

    ///Generates a new expression and returns it's id on the current context
    fn generate_expr(&mut self, expr: HirExpression) -> usize {
        match expr.kind {
            HirExpressionKind::StringLiteral(s) => {
                let handle = self.strings.insert_string(&s);
                self.active_context()
                    .insert_expr(IntermediateExpr::StringLiteral(handle))
            }
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
                match expr.ty {
                    HirType::Float => match op {
                        Operator::Add => self
                            .active_context()
                            .insert_expr(IntermediateExpr::FloatAdd(lhs, rhs)),
                        Operator::Sub => self
                            .active_context()
                            .insert_expr(IntermediateExpr::FloatSub(lhs, rhs)),
                        Operator::Star => self
                            .active_context()
                            .insert_expr(IntermediateExpr::FloatMul(lhs, rhs)),
                        Operator::Slash => self
                            .active_context()
                            .insert_expr(IntermediateExpr::FloatDiv(lhs, rhs)),
                    },
                    HirType::Int => match op {
                        Operator::Add => self
                            .active_context()
                            .insert_expr(IntermediateExpr::IntAdd(lhs, rhs)),
                        Operator::Sub => self
                            .active_context()
                            .insert_expr(IntermediateExpr::IntSub(lhs, rhs)),
                        Operator::Star => self
                            .active_context()
                            .insert_expr(IntermediateExpr::IntMul(lhs, rhs)),
                        Operator::Slash => self
                            .active_context()
                            .insert_expr(IntermediateExpr::IntDiv(lhs, rhs)),
                    },
                    ty => {
                        unreachable!(
                            "binary operation cannot happen where resultant type id {ty:?}"
                        )
                    }
                }
            }
            HirExpressionKind::Identifier(name) => self
                .active_context()
                .insert_expr(IntermediateExpr::Read(name)),
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
    fn generate_child(&mut self, name: HirType, values: Vec<ElementValueDeclaration>) -> usize {
        let HirType::Reference { rf, .. } = name else {
            unreachable!();
        };
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
                ElementValueDeclaration::Js(js) => {
                    self.active_context().insert_js(js);
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
                id: rf,
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
                ElementValueDeclaration::Js(js) => self.active_context().insert_js(js),
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
        args: Vec<IntermediateType>,
        statments: Vec<HirStatment>,
        name: String,
        id: HirId,
    ) -> usize {
        let cidx = self.contexts.len();
        let ctx = IntermediateContext::new_function(id, name, args.len());
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

    pub fn generate(&mut self, hir: SlynxHir) {
        let decls = Monomorphizer::new().monomorphize(hir);

        for decl in decls {
            match decl.kind {
                HirDeclarationKind::Function { statments, name } => {
                    let HirType::Function { args, .. } = &decl.ty else {
                        unreachable!("Type of function decl should be function")
                    };
                    let args = args
                        .into_iter()
                        .map(|arg| self.retrieve_intermediate_type(arg))
                        .collect();
                    let ty = self.retrieve_intermediate_type(&decl.ty);
                    self.types.insert(decl.id, ty);
                    self.generate_function(args, statments, name, decl.id);
                }
                HirDeclarationKind::ElementDeclaration { props } => {
                    let HirType::Component { props: ref decl_ty } = decl.ty else {
                        unreachable!("Type of element decl should be component");
                    };
                    for (_, _, ty) in decl_ty {
                        println!("{ty:?}");
                    }

                    self.generate_element(decl.id, props);
                }
            }
        }
    }
}
