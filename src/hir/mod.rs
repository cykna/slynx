pub mod declarations;
pub mod deffinitions;
pub mod error;
pub mod id; // New module for specific IDs
mod implementation;
pub mod names;
mod scopes;
pub mod symbols;
pub mod types;

use std::{collections::HashMap, sync::atomic::AtomicU64};

use color_eyre::eyre::Result;

use crate::{
    hir::{
        declarations::DeclarationsModule,
        deffinitions::{
            ComponentMemberDeclaration, HirDeclaration, HirDeclarationKind, HirStatment,
            HirStatmentKind,
        },
        error::{HIRError, HIRErrorKind},
        scope::HIRScope,
        scopes::ScopeModule,
        symbols::SymbolsModule,
        types::{HirType, TypesModule},
    },
    parser::ast::{
        ASTDeclaration, ASTDeclarationKind, ASTStatmentKind, ComponentMemberKind,
        ComponentMemberValue, Span, VisibilityModifier,
    },
};

// Re-export new ID types for convenience
pub use id::{DeclarationId, ExpressionId, PropertyId, TypeId, VariableId};

// Keep old HirId temporarily for backward compatibility during migration
static ACCUMULATOR: AtomicU64 = AtomicU64::new(0);

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[deprecated(
    since = "0.1.0",
    note = "Use specific ID types (DeclarationId, ExpressionId, etc.) instead"
)]
/// An ID for name resolution on the HIR
/// DEPRECATED: Use specific ID types instead
pub struct HirId(pub u64);

impl Default for HirId {
    fn default() -> Self {
        HirId::new()
    }
}

impl HirId {
    pub fn new() -> Self {
        Self(ACCUMULATOR.fetch_add(1, std::sync::atomic::Ordering::Relaxed))
    }
}

#[derive(Debug, Default)]
pub struct SlynxHir {
    ///The module that will keep track of all declarations on the top level
    declarations_module: DeclarationsModule,
    symbols_module: SymbolsModule,
    types_module: TypesModule,
    scope_module: ScopeModule,

    /// Maps the types of top level things on the current scope to their types.
    /// An example is functions, which contain an HirType.
    types: HashMap<TypeId, HirType>,
    /// A hashmap mapping the id of some struct or object to its layout. The 'layout' in case is the name of the property. So something like `object Packet {data: [100]u8, ty: PacketTy} would be simply
    /// id => ['data', 'ty'] to resolve its order correctly if some object expression like Packet(ty:PacketTy::Crypto, data:[100]0) appears
    pub(crate) objects_deffinitions: HashMap<HirId, Vec<String>>,
    /// The scopes of this HIR. On the final it's expected to have only one, which is the global one
    pub declarations: Vec<HirDeclaration>,
}

impl SlynxHir {
    pub fn new() -> Self {
        Self {
            scope_module: ScopeModule::new(),
            objects_deffinitions: HashMap::new(),
            types: HashMap::new(),
            declarations: Vec::new(),
            declarations_module: DeclarationsModule::new(),
            symbols_module: SymbolsModule::new(),
            types_module: TypesModule::new(),
        }
    }

    /// Generates the declarations from the provided `ast`
    pub fn generate(&mut self, ast: Vec<ASTDeclaration>) -> Result<()> {
        for ast in &ast {
            self.hoist(ast)?;
        }
        for ast in ast {
            self.resolve(ast)?;
        }
        Ok(())
    }

    /// Retrieves the id of the variable with the provided `name` going from the global scope, to the most internal one
    pub fn retrieve_variable_id(&mut self, name: &str, span: &Span) -> Result<VariableId> {
        let Some(symbol) = self.symbols_module.retrieve(name) else {
            return Err(HIRError {
                kind: HIRErrorKind::NameNotRecognized(name.to_string()),
                span: span.clone(),
            }
            .into());
        };
        let mut idx = self.scope_module.len() - 1;
        while idx != 0 {
            let scope = &self.scope_module[idx];

            let Some(id) = scope.retrieve_name(symbol) else {
                idx -= 1;
                continue;
            };
            return Ok(*id);
        }
        Err(HIRError {
            kind: HIRErrorKind::NameNotRecognized(name.to_string()),
            span: span.clone(),
        }
        .into())
    }

    /// Resolves the provided values on a component. The `ty` is the type of the component we are resolving it
    fn resolve_component_members(
        &mut self,
        members: Vec<ComponentMemberValue>,
        ty: TypeId,
    ) -> Result<Vec<ComponentMemberDeclaration>> {
        let mut out = Vec::with_capacity(members.len());
        for member in members {
            out.push(match member {
                ComponentMemberValue::Assign {
                    prop_name,
                    rhs,
                    span,
                } => {
                    let t = self.types_module.get_type(&ty);
                    let HirType::Component { props } = t else {
                        unreachable!("The type should be a component instead");
                    };
                    let index =
                        props
                            .iter()
                            .position(|prop| prop.1 == prop_name)
                            .ok_or(HIRError {
                                kind: HIRErrorKind::NameNotRecognized(prop_name.clone()),
                                span: span.clone(),
                            })?;

                    if matches!(
                        props[index].0,
                        VisibilityModifier::Private | VisibilityModifier::ChildrenPublic
                    ) {
                        return Err(HIRError {
                            kind: HIRErrorKind::PropertyNotVisible { prop_name },
                            span,
                        }
                        .into());
                    }
                    ComponentMemberDeclaration::Property {
                        id: PropertyId::new(), // Changed to PropertyId
                        index,
                        value: Some(self.resolve_expr(rhs, Some(props[index].2))?),
                        span,
                    }
                }
                ComponentMemberValue::Child(child) => {
                    //By now this won't track whether it can or cannot have children, since a method better than 'children' might be implemented in the future.
                    {
                        let (id, _) = {
                            self.retrieve_information_of_type(
                                &child.name.identifier,
                                &child.name.span,
                            )?
                        };
                        let values = self.resolve_component_members(child.values, id)?;

                        ComponentMemberDeclaration::Child {
                            name: id,
                            values,
                            span: child.span,
                        }
                    }
                    //else {
                    //     return Err(HIRError {
                    //         span: child.span.clone(),
                    //         kind: HIRErrorKind::InvalidChild {
                    //             child: Box::new(child),
                    //         },
                    //     }
                    //     .into());
                    // }
                }
            });
        }
        Ok(out)
    }

    /// Hoist the provided `ast` declaration, so no errors of undefined values because declared later may occur
    fn hoist(&mut self, ast: &ASTDeclaration) -> Result<()> {
        match &ast.kind {
            ASTDeclarationKind::ObjectDeclaration { name, fields } => {
                self.hoist_object(name, fields)?
            }

            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                return_type,
                ..
            } => self.hoist_function(name, args, return_type)?,
            ASTDeclarationKind::ComponentDeclaration { name, members, .. } => {
                let props = {
                    let mut out = Vec::with_capacity(members.len());
                    for member in members {
                        match &member.kind {
                            ComponentMemberKind::Property {
                                name, modifier, ty, ..
                            } => {
                                out.push((
                                    modifier.clone(),
                                    name.clone(),
                                    if let Some(generic) = ty {
                                        *self
                                            .get_typeid_of_name(&generic.identifier, &member.span)?
                                    } else {
                                        self.types_module.infer_id()
                                    },
                                ));
                            }
                            ComponentMemberKind::Child(_) => {}
                        }
                    }
                    out
                };
                self.create_declaration(&name.identifier, HirType::Component { props });
            }
        }
        Ok(())
    }

    fn resolve(&mut self, ast: ASTDeclaration) -> Result<()> {
        match ast.kind {
            ASTDeclarationKind::ObjectDeclaration { name, fields } => {
                self.resolve_object(name, fields, ast.span)?
            }
            ASTDeclarationKind::FuncDeclaration {
                name,
                args,
                mut body,
                ..
            } => {
                let (decl, tyid) = if let Some(symb) =
                    self.symbols_module.retrieve(&name.identifier)
                    && let Some(data) = self
                        .declarations_module
                        .retrieve_declaration_data_by_name(symb)
                {
                    data
                } else {
                    return Err(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(name.identifier),
                        span: name.span,
                    }
                    .into());
                };

                self.scope_module.enter_scope();
                let args = args
                    .into_iter()
                    .map(|arg| self.create_variable(&arg.name))
                    .collect();

                let statments = if let Some(last) = body.pop() {
                    let mut statments = Vec::with_capacity(body.len());
                    for ast in body {
                        statments.push(self.resolve_statment(ast)?);
                    }
                    if let ASTStatmentKind::Expression(expr) = last.kind {
                        let expr = self.resolve_expr(expr, None)?;
                        statments.push(HirStatment {
                            span: expr.span.clone(),
                            kind: HirStatmentKind::Return { expr },
                        });
                    }
                    statments
                } else {
                    Vec::new()
                };

                self.declarations.push(HirDeclaration {
                    kind: HirDeclarationKind::Function {
                        statments,
                        args,
                        name: name.to_string(),
                    },
                    id: decl,
                    ty: tyid,
                    span: ast.span,
                });
                self.scope_module.exit_scope();
            }
            ASTDeclarationKind::ComponentDeclaration { members, name } => {
                self.scope_module.enter_scope();

                let (decl, ty) = if let Some(symbol) =
                    self.symbols_module.retrieve(&name.identifier)
                    && let Some(data) = self
                        .declarations_module
                        .retrieve_declaration_data_by_name(symbol)
                {
                    data
                } else {
                    return Err(HIRError {
                        kind: HIRErrorKind::NameNotRecognized(name.identifier),
                        span: ast.span,
                    }
                    .into());
                };

                let defs = self.resolve_component_defs(members)?;
                self.declarations.push(HirDeclaration {
                    id: decl,
                    kind: HirDeclarationKind::ComponentDeclaration { props: defs },
                    ty,
                    span: ast.span,
                });
                self.scope_module.exit_scope();
            }
        }
        Ok(())
    }
}
