use crate::{
    hir::{
        HirId,
        error::{HIRError, HIRErrorKind, InvalidTypeReason},
    },
    parser::ast::{GenericIdentifier, VisibilityModifier},
};

#[derive(Debug, Clone)]
pub enum HirType {
    Struct {
        fields: Vec<HirType>,
    },

    Vector {
        ty: Box<HirType>,
    },
    ///This reference type can be understood better explained like
    ///object Name<T> {
    ///  value: T
    ///}
    ///func f(): Name<int> {
    ///  Name {value: 5}
    ///}
    ///Here, Name is the reference to the object type 'Name' with generic being 'int'
    Reference {
        ///The reference to the type this type maps to
        rf: HirId,
        ///If its got a generic
        generics: Vec<HirType>,
    },
    
    ///The type of the Nth field on the struct/object with the provided `id`. If the struct is defined as
    /// struct S {a:int, b:str}, then Field(S_ID, 0) == int
    Field(HirId, usize),
    
    Function {
        args: Vec<HirType>,
        return_type: Box<HirType>,
    },
    ///A type used for floats. This is by default the type of js.
    Float,
    ///A type used for ints. There's no Uint because js is gay. The difference between this to floats is that this is limited to be 32bits
    ///and it's optimized to use alot of byte operation to make things faster
    Int,

    ///Equivalent type of `string` in js
    Str,

    GenericComponent,
    ///A type specific for components
    Component {
        props: Vec<(VisibilityModifier, String, HirType)>,
    },
    ///A type that represents no value
    Void,
    ///Type that must be resolved during type check
    Infer,
}

impl HirType {
    ///Tries to retrieve a value from its `gener`(ic) type
    pub fn new(gener: &GenericIdentifier) -> Result<Self, HIRError> {
        match gener.identifier.as_str() {
            "Component" => Ok(Self::GenericComponent),
            "void" => Ok(Self::Void),
            "int" => Ok(Self::Int),
            "float" => Ok(Self::Float),
            "str" => Ok(Self::Str),
            "Vector" => {
                let generic_ty = gener.generic.as_ref().ok_or(HIRError {
                    kind: HIRErrorKind::InvalidType {
                        ty: gener.to_string(),
                        reason: InvalidTypeReason::MissingGeneric,
                    },
                    span: gener.span.clone(),
                })?;
                Ok(Self::Vector {
                    ty: Box::new(Self::new(&generic_ty[0])?),
                })
            }
            _ => Err(HIRError {
                kind: HIRErrorKind::TypeNotRecognized(gener.to_string()),
                span: gener.span.clone(),
            }),
        }
    }
}
