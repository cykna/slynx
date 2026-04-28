//! Declaration Definitions
//!
//! This module defines the [`HirDeclaration`] structure and related types that
//! represent top-level declarations in the HIR.
//!
//! # Overview
//!
//! Declarations are the building blocks of a Slynx program. They include:
//! - Functions
//! - Components
//! - Objects (data structures)
//! - Type aliases
//!
//! Each declaration has:
//! - A unique [`DeclarationId`] for identification
//! - A [`TypeId`] representing its type
//! - A [`HirDeclarationKind`] describing what kind of declaration it is
//! - A source [`Span`] for error reporting
//!
//! # Key Types
//!
//! - [`HirDeclaration`] — The main declaration structure
//! - [`HirDeclarationKind`] — Enum of declaration kinds
//! - [`ComponentMemberDeclaration`] — Members of a component declaration

use common::{Span, SymbolPointer};

use crate::hir::{
    DeclarationId, PropertyId, TypeId, VariableId,
    model::{HirExpression, HirStatement},
};

#[derive(Debug)]
#[repr(C)]
pub enum SpecializedComponent {
    Text {
        text: Box<HirExpression>,
    },
    Div {
        children: Vec<ComponentMemberDeclaration>,
    },
}
impl SpecializedComponent {
    ///Creates a new specialized Text component with the given `text`
    pub fn new_text(text: HirExpression) -> Self {
        Self::Text {
            text: Box::new(text),
        }
    }
    ///Creates a new specialized Div component with the given `children`
    pub fn new_div(children: Vec<ComponentMemberDeclaration>) -> Self {
        Self::Div { children }
    }
}

/// A top-level declaration in the HIR.
///
/// Every function, component, object, or type alias in a Slynx program becomes
/// a `HirDeclaration` during HIR generation. Declarations are stored in the
/// [`super::SlynxHir::declarations`] vector and are the primary output of the
/// HIR generation phase.
///
/// # Fields
///
/// - `kind` — What kind of declaration this is (function, component, etc.)
/// - `id` — A unique identifier for this declaration
/// - `ty` — The type of this declaration (e.g., function signature)
/// - `span` — Source location for error reporting
///
/// # Examples
///
/// ```rust
/// # use slynx_frontend::hir::model::*;
/// # use common::{Span, SymbolPointer};
/// # use crate::slynx_frontend::hir::{DeclarationId, TypeId};
/// # let span = Span::default();
/// # let name: SymbolPointer = todo!();
/// # let ty = TypeId::from_raw(0);
/// # let id = DeclarationId::new();
///
/// // A function declaration
/// let func_decl = HirDeclaration::new_function(
///     vec![],      // statements
///     vec![],      // arguments
///     name,
///     span,
///     id,
///     ty,
/// );
///
/// // An object declaration
/// let obj_decl = HirDeclaration::new_object(id, ty, span);
///
/// // A type alias
/// let alias_decl = HirDeclaration::new_alias(id, ty, span);
/// ```
///
/// # See Also
///
/// - [`HirDeclarationKind`] — The different kinds of declarations
/// - [`super::SlynxHir::declarations`] — Where declarations are stored
#[derive(Debug)]
#[repr(C)]
pub struct HirDeclaration {
    /// What kind of declaration this is.
    pub kind: HirDeclarationKind,

    /// A unique identifier for this declaration.
    ///
    /// This ID is used to reference the declaration from other parts of the
    /// HIR (e.g., function calls reference the function's DeclarationId).
    pub id: DeclarationId,

    /// The type of this declaration.
    ///
    /// For functions, this is the function's signature type. For components,
    /// it's the component type. For objects and aliases, it's the object/alias type.
    pub ty: TypeId,

    /// The source location of this declaration.
    ///
    /// Used for error reporting and diagnostics.
    pub span: Span,
}

/// The kind of a declaration.
///
/// This enum describes what kind of top-level declaration a [`HirDeclaration`]
/// represents. Each variant contains the data specific to that kind of declaration.
///
/// # Variants
///
/// ## `Object`
///
/// A simple data object declaration with no methods or behavior.
///
/// ```slynx
/// object Point { x: int, y: int }
/// ```
///
/// ## `Function`
///
/// A function declaration with a body, parameters, and name.
///
/// ```slynx
/// func add(a: int, b: int): int {
///     a + b
/// }
/// ```
///
/// ## `ComponentDeclaration`
///
/// A UI component with properties and children.
///
/// ```slynx
/// component Button(props: ButtonProps) {
///     prop label: str = "Click me";
/// }
/// ```
///
/// ## `Alias`
///
/// A type alias that creates a new name for an existing type.
///
/// ```slynx
/// type UserId = int;
/// ```
#[derive(Debug)]
#[repr(C)]
pub enum HirDeclarationKind {
    /// A simple data object with fields.
    Object,

    /// A function declaration.
    ///
    /// # Fields
    ///
    /// - `statements` — The function body as a sequence of statements
    /// - `args` — The function's parameter variables
    /// - `name` — The function's name as a symbol
    Function {
        /// The statements comprising the function body.
        statements: Vec<HirStatement>,

        /// The function's parameters, as variable IDs.
        args: Vec<VariableId>,

        /// The function's name.
        name: SymbolPointer,
    },

    /// A component declaration.
    ///
    /// # Fields
    ///
    /// - `name` — The component's name as a symbol
    /// - `props` — The component's property declarations
    ComponentDeclaration {
        /// The component's name.
        name: SymbolPointer,

        /// The component's properties.
        props: Vec<ComponentMemberDeclaration>,
    },

    /// A type alias declaration.
    ///
    /// Aliases create alternative names for existing types.
    Alias,
}

/// A member of a component declaration.
///
/// Component members can be either properties (with optional default values)
/// or child components.
///
/// # Variants
///
/// ## `Property`
///
/// A named property of the component with an optional default value.
///
/// ```slynx
/// prop label: str = "Hello"
/// ```
///
/// ## `Child`
///
/// A child component that can contain other members.
///
/// ```slynx
/// child Container {
///     prop items: list<str>
/// }
/// ```
///
/// ## `Specialized`
///
/// A specialized component like `Text` or `Div` with predefined behavior.
#[derive(Debug)]
#[repr(C)]
pub enum ComponentMemberDeclaration {
    /// A property declaration with an optional default value.
    ///
    /// # Fields
    ///
    /// - `id` — A unique ID for this property
    /// - `index` — The property's position in the component's property list
    /// - `value` — The optional default value expression
    /// - `span` — Source location for error reporting
    Property {
        /// Unique identifier for this property.
        id: PropertyId,

        /// The index of this property in the component's property list.
        ///
        /// Used for efficient property access at runtime.
        index: usize,

        /// The property's default value, if any.
        ///
        /// If `None`, the property must be provided when the component is used.
        value: Option<HirExpression>,

        /// The source location of this property declaration.
        span: Span,
    },

    /// A child component declaration.
    ///
    /// # Fields
    ///
    /// - `name` — The child component's type
    /// - `values` — The child's property values
    /// - `span` — Source location for error reporting
    Child {
        /// The type of the child component.
        name: TypeId,

        /// The property values passed to the child component.
        values: Vec<ComponentMemberDeclaration>,

        /// The source location of this child declaration.
        span: Span,
    },

    /// A specialized component with predefined behavior.
    ///
    /// Specialized components like `Text` and `Div` have built-in rendering
    /// logic and don't require full component definitions.
    Specialized(super::SpecializedComponent),
}

impl ComponentMemberDeclaration {
    /// Creates a new property declaration.
    ///
    /// # Arguments
    ///
    /// * `index` — The property's position in the component's property list
    /// * `value` — The optional default value expression
    /// * `span` — The source location of the property
    ///
    /// # Returns
    ///
    /// A new [`ComponentMemberDeclaration::Property`] instance.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use slynx_frontend::hir::model::*;
    /// # use common::Span;
    /// # let span = Span::default();
    /// # let value = None;
    /// let prop = ComponentMemberDeclaration::new_property(
    ///     0,      // index
    ///     value,  // default value
    ///     span,
    /// );
    /// ```
    pub fn new_property(index: usize, value: Option<HirExpression>, span: Span) -> Self {
        Self::Property {
            id: PropertyId::new(),
            index,
            value,
            span,
        }
    }

    /// Creates a new child component declaration.
    ///
    /// # Arguments
    ///
    /// * `name` — The child component's type ID
    /// * `values` — The child's property values
    /// * `span` — The source location of the child declaration
    ///
    /// # Returns
    ///
    /// A new [`ComponentMemberDeclaration::Child`] instance.
    ///
    /// # Example
    ///
    /// ```rust
    /// # use slynx_frontend::hir::model::*;
    /// # use common::Span;
    /// # let span = Span::default();
    /// # let name = TypeId::from_raw(0);
    /// # let values = vec![];
    /// let child = ComponentMemberDeclaration::new_child(name, values, span);
    /// ```
    pub fn new_child(name: TypeId, values: Vec<ComponentMemberDeclaration>, span: Span) -> Self {
        Self::Child { name, values, span }
    }
}

impl HirDeclaration {
    /// Creates a new function declaration.
    ///
    /// # Arguments
    ///
    /// * `statements` — The function body statements
    /// * `args` — The function's parameter variable IDs
    /// * `name` — The function's name as a symbol
    /// * `span` — The source location of the function declaration
    /// * `id` — The declaration's unique ID
    /// * `ty` — The function's type ID (signature)
    ///
    /// # Returns
    ///
    /// A new [`HirDeclaration`] with [`HirDeclarationKind::Function`].
    ///
    /// # Example
    ///
    /// ```rust
    /// # use slynx_frontend::hir::model::*;
    /// # use common::{Span, SymbolPointer};
    /// # use crate::slynx_frontend::hir::{DeclarationId, TypeId};
    /// # let span = Span::default();
    /// # let name: SymbolPointer = todo!();
    /// # let ty = TypeId::from_raw(0);
    /// # let id = DeclarationId::new();
    /// let func = HirDeclaration::new_function(
    ///     vec![],      // statements
    ///     vec![],      // args
    ///     name,
    ///     span,
    ///     id,
    ///     ty,
    /// );
    /// ```
    pub fn new_function(
        statements: Vec<HirStatement>,
        args: Vec<VariableId>,
        name: SymbolPointer,
        span: Span,
        id: DeclarationId,
        ty: TypeId,
    ) -> Self {
        Self {
            kind: HirDeclarationKind::Function {
                statements,
                args,
                name,
            },
            span,
            id,
            ty,
        }
    }

    /// Creates a new object declaration.
    ///
    /// # Arguments
    ///
    /// * `decl` — The declaration's unique ID
    /// * `declty` — The object's type ID
    /// * `span` — The source location of the object declaration
    ///
    /// # Returns
    ///
    /// A new [`HirDeclaration`] with [`HirDeclarationKind::Object`].
    ///
    /// # Example
    ///
    /// ```rust
    /// # use slynx_frontend::hir::model::*;
    /// # use common::Span;
    /// # use crate::slynx_frontend::hir::{DeclarationId, TypeId};
    /// # let span = Span::default();
    /// # let id = DeclarationId::new();
    /// # let ty = TypeId::from_raw(0);
    /// let obj = HirDeclaration::new_object(id, ty, span);
    /// ```
    pub fn new_object(decl: DeclarationId, declty: TypeId, span: Span) -> Self {
        Self {
            kind: HirDeclarationKind::Object,
            id: decl,
            ty: declty,
            span,
        }
    }

    /// Creates a new type alias declaration.
    ///
    /// # Arguments
    ///
    /// * `decl` — The alias declaration's unique ID
    /// * `ty` — The target type ID that the alias refers to
    /// * `span` — The source location of the alias declaration
    ///
    /// # Returns
    ///
    /// A new [`HirDeclaration`] with [`HirDeclarationKind::Alias`].
    ///
    /// # Example
    ///
    /// ```rust
    /// # use slynx_frontend::hir::model::*;
    /// # use common::Span;
    /// # use crate::slynx_frontend::hir::{DeclarationId, TypeId};
    /// # let span = Span::default();
    /// # let id = DeclarationId::new();
    /// # let ty = TypeId::from_raw(0);
    /// let alias = HirDeclaration::new_alias(id, ty, span);
    /// ```
    pub fn new_alias(decl: DeclarationId, ty: TypeId, span: Span) -> Self {
        Self {
            id: decl,
            kind: HirDeclarationKind::Alias,
            ty,
            span,
        }
    }
}
