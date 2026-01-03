pub mod context;
pub mod expr;
pub mod node;
pub mod string;
pub mod types;
use std::collections::HashMap;

use crate::{
    hir::{
        HirId,
        deffinitions::{
            ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind, SpecializedComponent,
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
#[derive(Debug, Default)]
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
            HirExpressionKind::Component { name, values, .. } => {
                let eidx = self.generate_child(name, values);
                if matches!(
                    self.active_context().ty,
                    IntermediateContextType::Component { .. }
                ) {
                    self.active_context().insert_child(eidx); //child index
                };
                eidx
            }
            HirExpressionKind::Specialized(spec) => {
                let sidx = self.generate_specialized(spec);
                if matches!(
                    self.active_context().ty,
                    IntermediateContextType::Component { .. }
                ) {
                    self.active_context().insert_child(sidx);
                }
                sidx
            }
            HirExpressionKind::Object { name, fields } => {
                let indexes = fields
                    .into_iter()
                    .map(|expr| self.generate_expr(expr))
                    .collect();
                self.active_context().insert_expr(IntermediateExpr::Struct {
                    id: name,
                    exprs: indexes,
                })
            }
            HirExpressionKind::FieldAccess { expr, field_index } => {
                let expr = self.generate_expr(*expr);
                self.active_context()
                    .insert_expr(IntermediateExpr::FieldAccess {
                        parent: expr,
                        field: field_index,
                    })
            }
        }
    }
    ///Creates a new child on the current context and returns the component expression and the child id
    fn generate_child(&mut self, name: HirId, values: Vec<ComponentMemberDeclaration>) -> usize {
        let mut props = Vec::new();
        let mut children = Vec::new();

        for value in values {
            match value {
                ComponentMemberDeclaration::Property { index, value, .. } => {
                    let out = value.map(|value| self.generate_expr(value));
                    for _ in 0..index - props.len() {
                        props.push(None);
                    }
                    props.push(out);
                }
                ComponentMemberDeclaration::Child { name, values, .. } => {
                    let idx = self.generate_child(name, values);
                    children.push(idx);
                }
                ComponentMemberDeclaration::Specialized(spec) => {
                    let idx = self.generate_specialized(spec);
                    children.push(idx);
                }
            }
        }
        //expr index
        let eidx = self
            .active_context()
            .insert_expr(IntermediateExpr::Component {
                id: name,
                props,
                children,
            });
        self.active_context().insert_child(eidx);

        eidx
    }

    fn generate_specialized(&mut self, spec: SpecializedComponent) -> usize {
        let expr = match spec {
            SpecializedComponent::Text { text } => {
                let txt = self.generate_expr(*text);
                IntermediateExpr::native_text(txt, Vec::new())
            }
            SpecializedComponent::Div { children } => {
                let mut props = Vec::new();
                let children = children
                    .into_iter()
                    .filter_map(|v| match v {
                        ComponentMemberDeclaration::Child { name, values, .. } => {
                            Some(self.generate_child(name, values))
                        }
                        ComponentMemberDeclaration::Property { index, value, .. } => {
                            let out = value.map(|v| self.generate_expr(v));
                            for _ in 0..index - props.len() {
                                props.push(None);
                            }
                            out
                        }
                        ComponentMemberDeclaration::Specialized(spec) => {
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

    ///Creates a new component with the provided informations. Returns the component id and it's type id
    fn generate_component(
        &mut self,
        id: HirId,
        props: Vec<ComponentMemberDeclaration>,
        tys: &[IntermediateType],
    ) {
        let expr = IntermediateContext::new_component(id);

        self.contexts.push(expr);
        for (idx, prop) in props.into_iter().enumerate() {
            match prop {
                ComponentMemberDeclaration::Property { value, id, .. } => {
                    let default_value = value.map(|value| self.generate_expr(value));
                    self.active_context().insert_property(IntermediateProperty {
                        id,
                        default_value,
                        ty: tys[idx].clone(),
                    });
                }
                ComponentMemberDeclaration::Specialized(spec) => {
                    self.generate_specialized(spec);
                }
                ComponentMemberDeclaration::Child { name, values, .. } => {
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
                HirDeclarationKind::Object => {
                    let HirType::Struct { fields } = decl.ty else {
                        unreachable!("Type of a object declaration should be 'struct'");
                    };
                    let ty = self.retrieve_complex(&fields);
                    self.types_mapping.insert(decl.id, ty);
                }
                HirDeclarationKind::Function { statments, name } => {
                    let HirType::Function { args, return_type } = &decl.ty else {
                        unreachable!("Type of function decl should be function")
                    };

                    let args = args.iter().map(|arg| self.get_type(arg)).collect();
                    let ret = self.get_type(return_type);
                    self.generate_function(args, ret, statments, name, decl.id);
                }
                HirDeclarationKind::ComponentDeclaration { props } => {
                    let mut tys = Vec::new();
                    let HirType::Component { props: ref decl_ty } = decl.ty else {
                        unreachable!("Type of component decl should be component");
                    };
                    for (_, _, ty) in decl_ty {
                        let typ = self.get_type(ty);
                        tys.push(typ);
                    }

                    self.generate_component(decl.id, props, &tys);
                    self.types_mapping
                        .insert(decl.id, IntermediateType::Complex(tys));
                }
            }
        }
    }
}
