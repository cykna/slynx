pub mod context;
pub mod expr;
pub mod node;
pub mod string;
pub mod types;
use std::collections::HashMap;

use crate::{
    hir::{
        HirId,
        declaration::{
            ElementValueDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind, SpecializedElement,
        },
        types::HirType,
    },
    intermediate::{
        context::{IntermediateContext, IntermediateContextType, IntermediateProperty},
        expr::IntermediateExpr,
        string::StringPool,
        types::IntermediateType,
    },
};

///Struct used to represent the intermediate representation of Slynx. WIll be used when being compiled. This contains the monomorfization of types
///and flat values of everything in the source code. It can be understood as the context itself of the IR
pub struct IntermediateRepr {
    pub contexts: Vec<IntermediateContext>,
    pub types_mapping: HashMap<HirId, IntermediateType>,
    pub strings: StringPool,
}

impl IntermediateRepr {
    pub fn new() -> Self {
        Self {
            contexts: Vec::new(),
            types_mapping: HashMap::new(),
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
            HirExpressionKind::Specialized(spec) => {
                let sidx = self.generate_specialized(spec);
                if matches!(
                    self.active_context().ty,
                    IntermediateContextType::Element { .. }
                ) {
                    self.active_context().insert_child(sidx);
                }
                sidx
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
                ElementValueDeclaration::Specialized(spec) => {
                    let idx = self.generate_specialized(spec);
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
        self.active_context().insert_child(eidx);

        eidx
    }

    fn generate_specialized(&mut self, spec: SpecializedElement) -> usize {
        let expr = match spec {
            SpecializedElement::Text { text } => {
                let txt = self.generate_expr(*text);
                IntermediateExpr::native_text(txt, Vec::new())
            }
            SpecializedElement::Div { children } => {
                let mut props = Vec::new();
                let children = children
                    .into_iter()
                    .filter_map(|v| match v {
                        ElementValueDeclaration::Child { name, values, .. } => {
                            Some(self.generate_child(name, values))
                        }
                        ElementValueDeclaration::Property { index, value, .. } => {
                            let out = value.map(|v| self.generate_expr(v));
                            for _ in 0..index - props.len() {
                                props.push(None);
                            }
                            out
                        }
                        ElementValueDeclaration::Specialized(spec) => {
                            Some(self.generate_specialized(spec))
                        }
                    })
                    .collect();
                IntermediateExpr::native_rect(children, props)
            }
        };
        let sidx = self.active_context().insert_expr(expr);
        self.active_context().insert_child(sidx);
        sidx
    }

    ///Creates a new element with the provided informations. Returns the component id and it's type id
    fn generate_element(
        &mut self,
        id: HirId,
        props: Vec<ElementValueDeclaration>,
        tys: &Vec<IntermediateType>,
    ) {
        let expr = IntermediateContext::new_element(id);

        self.contexts.push(expr);
        for (idx, prop) in props.into_iter().enumerate() {
            match prop {
                ElementValueDeclaration::Property { value, id, .. } => {
                    let default_value = if let Some(value) = value {
                        Some(self.generate_expr(value))
                    } else {
                        None
                    };
                    self.active_context().insert_property(IntermediateProperty {
                        id,
                        default_value,
                        ty: tys[idx].clone(),
                    });
                }
                ElementValueDeclaration::Specialized(spec) => {
                    self.generate_specialized(spec);
                }
                ElementValueDeclaration::Child { name, values, .. } => {
                    self.generate_child(name, values);
                }
            }
        }
    }

    ///Creates a new function with the provided `name` `id` and `statments` and returns its id
    pub fn generate_function(
        &mut self,
        args: Vec<IntermediateType>,
        ret: IntermediateType,
        statments: Vec<HirStatment>,
        name: String,
        id: HirId,
    ) -> usize {
        let cidx = self.contexts.len();
        let ctx = IntermediateContext::new_function(id, name, args, ret);
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

                    let args = args.iter().map(|arg| self.get_type(arg)).collect();
                    let ret = self.get_type(return_type);
                    self.generate_function(args, ret, statments, name, decl.id);
                }
                HirDeclarationKind::ElementDeclaration { props } => {
                    let mut tys = Vec::new();
                    let HirType::Component { props: ref decl_ty } = decl.ty else {
                        unreachable!("Type of element decl should be component");
                    };
                    for (_, _, ty) in decl_ty {
                        let typ = self.get_type(ty);
                        tys.push(typ);
                    }

                    self.generate_element(decl.id, props, &tys);
                    self.types_mapping
                        .insert(decl.id, IntermediateType::Complex(tys));
                }
            }
        }
    }
}
