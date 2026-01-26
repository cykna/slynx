pub mod context;
pub mod expr;
pub mod id;
pub mod node;
pub mod string;
pub mod types;
use std::collections::HashMap;

use crate::{
    hir::{
        TypeId, VariableId,
        deffinitions::{
            ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, HirExpression,
            HirExpressionKind, HirStatment, HirStatmentKind, SpecializedComponent,
        },
        types::{HirType, TypesModule},
    },
    intermediate::{
        context::{IntermediateContext, IntermediateContextType, IntermediateProperty},
        expr::IntermediateExpr,
        id::{GlobalId, TyId, ValueId},
        node::{IntermediateInstruction, IntermediatePlace},
        string::StringPool,
        types::IntermediateType,
    },
};

#[derive(Debug, Default)]
/// Struct used to represent the intermediate representation of Slynx. Will be used when being compiled. This contains the monomorfization of types
/// and flat values of everything in the source code. It can be understood as the context itself of the IR
pub struct IntermediateRepr {
    pub contexts: Vec<IntermediateContext>,
    pub types_mapping: HashMap<TyId, IntermediateType>,
    ///Mapping of high level types to low level types
    pub types: HashMap<TypeId, TyId>,
    pub strings: StringPool,
}

impl IntermediateRepr {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
            contexts: Vec::new(),
            types_mapping: HashMap::new(),
            strings: StringPool::new(),
        }
    }

    fn active_context(&mut self) -> &mut IntermediateContext {
        let len = self.contexts.len();
        &mut self.contexts[len - 1]
    }

    /// Generates a new expression and returns it's id on the current context
    fn generate_expr(&mut self, expr: HirExpression) -> ValueId {
        match expr.kind {
            HirExpressionKind::StringLiteral(s) => {
                let handle = self.strings.insert_string(&s);
                self.active_context()
                    .insert_expr(IntermediateExpr::str(handle))
            }
            HirExpressionKind::Int(int) => self
                .active_context()
                .insert_expr(IntermediateExpr::int(int)),
            HirExpressionKind::Float(float) => self
                .active_context()
                .insert_expr(IntermediateExpr::float(float)),

            HirExpressionKind::Binary { lhs, op, rhs } => {
                let lhs = self.generate_expr(*lhs);
                let rhs = self.generate_expr(*rhs);
                self.active_context()
                    .insert_expr(IntermediateExpr::bin(lhs, rhs, op))
            }
            HirExpressionKind::Identifier(name) => self
                .active_context()
                .insert_expr(IntermediateExpr::identifier(name)),
            HirExpressionKind::Component { name, values, .. } => {
                let name = self.types.get(&name).unwrap().clone();
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
                let name = *self.types.get(&name).unwrap();
                self.active_context()
                    .insert_expr(IntermediateExpr::strct(name, indexes))
            }
            HirExpressionKind::FieldAccess { expr, field_index } => {
                let expr = self.generate_expr(*expr);
                self.active_context()
                    .insert_expr(IntermediateExpr::field(expr, field_index))
            }
        }
    }

    /// Creates a new child on the current context and returns the component expression and the child id
    fn generate_child(&mut self, name: TyId, values: Vec<ComponentMemberDeclaration>) -> ValueId {
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
                    let name = self.types.get(&name).unwrap().clone();
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
            .insert_expr(IntermediateExpr::component(name, props, children));
        self.active_context().insert_child(eidx);
        eidx
    }

    fn generate_specialized(&mut self, spec: SpecializedComponent) -> ValueId {
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
                            let name = *self.types.get(&name).unwrap();
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

    /// Creates a new component with the provided informations. Returns the component id and it's type id
    fn generate_component(
        &mut self,
        props: Vec<ComponentMemberDeclaration>,
        tys: &[IntermediateType],
    ) {
        let expr = IntermediateContext::new_component();

        self.contexts.push(expr);
        for (idx, prop) in props.into_iter().enumerate() {
            match prop {
                ComponentMemberDeclaration::Property { value, .. } => {
                    let default_value = value.map(|value| self.generate_expr(value));
                    self.active_context().insert_property(IntermediateProperty {
                        id: GlobalId::new(),
                        default_value,
                        ty: tys[idx].clone(),
                    });
                }
                ComponentMemberDeclaration::Specialized(spec) => {
                    self.generate_specialized(spec);
                }
                ComponentMemberDeclaration::Child { name, values, .. } => {
                    let name = *self.types.get(&name).unwrap();
                    self.generate_child(name, values);
                }
            }
        }
    }

    pub fn generate_var(&mut self, name: crate::hir::VariableId, value: HirExpression) -> usize {
        let idx = self.generate_expr(value);
        self.active_context()
            .insert_instruction(IntermediateInstruction::mov(
                IntermediatePlace::Local(name),
                idx,
            ))
            .unwrap()
    }

    fn generate_place(&mut self, lhs: HirExpression) -> IntermediatePlace {
        match lhs.kind {
            HirExpressionKind::Identifier(id) => IntermediatePlace::Local(
                *self
                    .active_context()
                    .vars
                    .iter()
                    .find(|var| **var == id)
                    .unwrap(),
            ),
            HirExpressionKind::FieldAccess { field_index, expr } => {
                let _structure_place = self.generate_place(*expr);
                IntermediatePlace::Field {
                    parent: VariableId::new(),
                    field: field_index,
                }
            }
            _ => panic!("Unsupported expression type for place generation: {lhs:?}"),
        }
    }

    pub fn generate_assign(&mut self, lhs: HirExpression, value: HirExpression) -> usize {
        let idx = self.generate_expr(value);
        let place = self.generate_place(lhs);
        self.active_context()
            .insert_instruction(IntermediateInstruction::mov(place, idx))
            .unwrap()
    }

    /// Creates a new function with the provided `name` `id` and `statments` and returns its id
    pub fn generate_function(
        &mut self,
        args: Vec<IntermediateType>,
        ret: IntermediateType,
        statments: Vec<HirStatment>,
        name: String,
    ) -> usize {
        let cidx = self.contexts.len();
        let ctx = IntermediateContext::new_function(name, args, ret);
        self.contexts.push(ctx);
        for statment in statments {
            match statment.kind {
                HirStatmentKind::Assign { lhs, value } => {
                    self.generate_assign(lhs, value);
                }
                HirStatmentKind::Variable { name, value, .. } => {
                    self.generate_var(name, value); // Convert VariableId to HirId
                }
                HirStatmentKind::Expression { expr } => {
                    self.generate_expr(expr);
                }
                HirStatmentKind::Return { expr } => {
                    let id = self.generate_expr(expr);
                    self.active_context()
                        .insert_instruction(node::IntermediateInstruction::ret(id));
                }
            };
        }
        cidx
    }

    pub fn generate(&mut self, decls: Vec<HirDeclaration>, module: TypesModule) {
        for decl in decls {
            match decl.kind {
                HirDeclarationKind::Object => {
                    let HirType::Struct { fields } = module.get_type(&decl.ty) else {
                        unreachable!("Type of a object declaration should be 'struct'");
                    };
                    let ty = self.retrieve_complex(fields.as_slice(), &module);
                    let tyid = TyId::new();
                    self.types.insert(decl.ty, tyid);
                    self.types_mapping.insert(tyid, ty); // Convert DeclarationId to HirId
                }
                HirDeclarationKind::Function {
                    statments, name, ..
                } => {
                    let HirType::Function {
                        args: tys,
                        return_type,
                    } = module.get_type(&decl.ty)
                    else {
                        unreachable!("Type of function decl should be function")
                    };

                    let argsty = tys.iter().map(|ty| self.get_type(ty, &module)).collect();
                    let ret = self.get_type(return_type, &module);
                    self.generate_function(argsty, ret, statments, name);
                }
                HirDeclarationKind::ComponentDeclaration { props } => {
                    let mut tys = Vec::new();
                    let HirType::Component { props: decl_ty } = module.get_type(&decl.ty) else {
                        unreachable!("Type of component decl should be component");
                    };
                    for (_, _, ty) in decl_ty {
                        let typ = self.get_type(ty, &module);
                        tys.push(typ);
                    }

                    self.generate_component(props, &tys); // Convert DeclarationId to HirId
                    let ty = TyId::new();
                    self.types.insert(decl.ty, ty);
                    self.types_mapping
                        .insert(ty, IntermediateType::Complex(tys));
                    // Convert DeclarationId to HirId
                }
            }
        }
    }
}
