use std::borrow::Cow;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug)]
///A name that is typed. This is simply the representation of `name: kind`
pub struct TypedName {
    pub name: String,
    pub kind: GenericIdentifier,
    pub span: Span,
}

#[derive(Debug)]
///A Identifier that might contain a generic. Such as Component<int>
pub struct GenericIdentifier {
    ///The generic this identifier contains.
    pub generic: Option<Box<GenericIdentifier>>,
    ///The name of this identifier
    pub identifier: String,
    pub span: Span,
}

impl std::fmt::Display for GenericIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(ref generic) = self.generic {
            write!(f, "{}<{}>", self.identifier, generic)
        } else {
            write!(f, "{}", self.identifier)
        }
    }
}

#[derive(Default, Debug, Clone)]
pub enum PropertyModifier {
    ///Property visible to everyone
    Public,
    ///Property visible only to the element itself.
    #[default]
    Private,
    ///Property visible only for the children
    ChildrenPublic,
    ///Property visible only for the parents
    ParentPublic,
}
#[derive(Debug)]
pub struct ElementDeffinition {
    pub kind: ElementDeffinitionKind,
    pub span: Span,
}
#[derive(Debug)]
pub enum ElementDeffinitionKind {
    RawJs(Cow<'static, str>),
    MacroCall {
        name: String,
        args: MacroElementArgs,
    },
    Property {
        name: String,
        modifier: PropertyModifier,
        ty: Option<GenericIdentifier>,
        rhs: Option<ASTExpression>,
    },
    Child(ElementExpression),
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub enum Operator {
    Add,
    Sub,
    Star,
    Slash,
}

#[derive(Debug)]
pub enum ElementValue {
    Assign {
        prop_name: String,
        rhs: ASTExpression,
        span: Span,
    },
    Element(ElementExpression),
}

#[derive(Debug)]
pub struct ElementExpression {
    pub name: GenericIdentifier,
    pub values: Vec<ElementValue>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ASTStatment {
    pub kind: ASTStatmentKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct MacroCallDecl {
    pub name: String,
    pub args: Vec<ASTDeclaration>,
}
#[derive(Debug)]
pub struct MacroCallStmt {
    pub name: String,
    pub args: Vec<ASTStatment>,
}

#[derive(Debug)]
///Arguments for a macro when being called inside a element deffinition.
///This is able to receive statments because it might be able to create functions with the
///provided ones
pub enum MacroElementArgs {
    Empty,
    Statments(Vec<ASTStatment>),
    Deffinitions(Vec<ElementDeffinition>),
}

#[derive(Debug)]
pub struct MacroCallElement {
    pub name: String,
    pub args: MacroElementArgs,
}

#[derive(Debug)]
pub enum ASTStatmentKind {
    MacroCall(MacroCallStmt),
    Var {
        name: String,
        ty: Option<GenericIdentifier>,
        rhs: ASTExpression,
    },
    MutableVar {
        name: String,
        ty: Option<GenericIdentifier>,
        rhs: ASTExpression,
    },
    Expression(ASTExpression),
}

#[derive(Debug)]
pub struct ASTExpression {
    pub kind: ASTExpressionKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTExpressionKind {
    Element(ElementExpression),
    IntLiteral(i32),
    Uint8x4Literal(
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
    ),
    Int8x4Literal(
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
        Box<ASTExpression>,
    ),
    Int16x2Literal(Box<ASTExpression>, Box<ASTExpression>),
    Uint16x2Literal(Box<ASTExpression>, Box<ASTExpression>),
    Binary {
        lhs: Box<ASTExpression>,
        op: Operator,
        rhs: Box<ASTExpression>,
    },
    Identifier(String),
    FloatLiteral(f32),
}
#[derive(Debug)]
pub struct ASTDeclaration {
    pub kind: ASTDeclarationKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum ASTDeclarationKind {
    MacroCall(MacroCallDecl),
    ElementDeclaration {
        name: String,
        deffinitions: Vec<ElementDeffinition>,
    },
    FuncDeclaration {
        name: String,
        args: Vec<TypedName>,
        return_type: GenericIdentifier,
        body: Vec<ASTStatment>,
    },
}
