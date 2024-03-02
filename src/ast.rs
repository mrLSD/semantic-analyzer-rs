//! # AST
//! Abstract Syntax Tree representation contains tree node that
//! represent full cycle and aspects of the programming language, and
//! represent `Turing-complete` state machine.

use crate::types::semantic::{ExtendedExpression, SemanticContextInstruction};
use nom_locate::LocatedSpan;
#[cfg(feature = "codec")]
use serde::{
    de::{self, Deserializer, MapAccess, Visitor},
    ser::{SerializeStruct, Serializer},
    Deserialize, Serialize,
};
use std::convert::Infallible;
use std::marker::PhantomData;

/// Max priority level fpr expressions operations
pub const MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS: u8 = 9;

/// Basic `Ident` entity for elements of AST
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub struct Ident<'a>(LocatedSpan<&'a str>);

/// Ident methods mirroring `LocatedSpan`
impl<'a> Ident<'a> {
    #[must_use]
    pub fn new(ident: &'a str) -> Self {
        Self(LocatedSpan::new(ident))
    }

    #[must_use]
    pub fn fragment(&self) -> &'a str {
        self.0.fragment()
    }

    #[must_use]
    pub fn location_line(&self) -> u32 {
        self.0.location_line()
    }

    #[must_use]
    pub fn location_offset(&self) -> usize {
        self.0.location_offset()
    }
}

impl<'a> std::fmt::Display for Ident<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.fragment())
    }
}

impl<'a> From<&'a str> for Ident<'a> {
    fn from(value: &'a str) -> Self {
        Ident::new(value)
    }
}

/// Ident Serializer
#[cfg(feature = "codec")]
impl<'a> Serialize for Ident<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let fragment = self.0.into_fragment_and_extra();
        let mut state = serializer.serialize_struct("Ident", 4)?;
        state.serialize_field("offset", &self.0.location_offset())?;
        state.serialize_field("line", &self.0.location_line())?;
        state.serialize_field("fragment", &fragment.0)?;
        state.serialize_field("extra", &fragment.1)?;
        state.end()
    }
}

/// Ident Deserializer
#[cfg(feature = "codec")]
impl<'de: 'a, 'a> Deserialize<'de> for Ident<'a> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        // grcov-excl-start
        struct IdentVisitor<'a> {
            marker: std::marker::PhantomData<fn() -> Ident<'a>>,
        }

        impl<'de: 'a, 'a> Visitor<'de> for IdentVisitor<'a> {
            type Value = Ident<'de>;

            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("struct Ident")
            }

            fn visit_map<V>(self, mut map: V) -> Result<Ident<'de>, V::Error>
            where
                V: MapAccess<'de>,
            {
                let mut offset = None;
                let mut line = None;
                let mut fragment = None;
                let mut extra = None;
                while let Some(key) = map.next_key::<String>()? {
                    match key.as_str() {
                        "offset" => {
                            if offset.is_some() {
                                return Err(de::Error::duplicate_field("offset"));
                            }
                            offset = Some(map.next_value()?);
                        }
                        "line" => {
                            if line.is_some() {
                                return Err(de::Error::duplicate_field("line"));
                            }
                            line = Some(map.next_value()?);
                        }
                        "fragment" => {
                            if fragment.is_some() {
                                return Err(de::Error::duplicate_field("fragment"));
                            }
                            fragment = Some(map.next_value()?);
                        }
                        "extra" => {
                            if extra.is_some() {
                                return Err(de::Error::duplicate_field("extra"));
                            }
                            extra = Some(map.next_value()?);
                        }
                        _ => return Err(de::Error::unknown_field(&key, FIELDS)),
                    }
                }
                let offset = offset.ok_or_else(|| de::Error::missing_field("offset"))?;
                let line = line.ok_or_else(|| de::Error::missing_field("line"))?;
                let fragment = fragment.ok_or_else(|| de::Error::missing_field("fragment"))?;
                #[allow(clippy::let_unit_value)]
                let extra = extra.ok_or_else(|| de::Error::missing_field("extra"))?;
                let located =
                    unsafe { LocatedSpan::new_from_raw_offset(offset, line, fragment, extra) };
                Ok(Ident(located))
            }
            // grcov-excl-end
        }

        const FIELDS: &[&str] = &["offset", "line", "fragment", "extra"];
        deserializer.deserialize_struct(
            "Ident",
            FIELDS,
            IdentVisitor {
                marker: std::marker::PhantomData,
            },
        )
    }
}

/// `GetName` trait, represent name of specific entity.
pub trait GetName {
    fn name(&self) -> String;
}

/// `GetLocation` represent location of source data for AST element.
/// Useful to locate specific source code location, especially for `Ident`.
pub trait GetLocation {
    fn location(&self) -> CodeLocation;
}

/// Import name element of AST
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ImportName<'a>(#[cfg_attr(feature = "codec", serde(borrow))] Ident<'a>);

impl GetName for ImportName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

impl<'a> ImportName<'a> {
    #[must_use]
    pub const fn new(ident: Ident<'a>) -> Self {
        Self(ident)
    }
}

/// Imports with full path of import
pub type ImportPath<'a> = Vec<ImportName<'a>>;

/// `ConstantName` constant name for `Constant` elements of AST
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ConstantName<'a>(#[cfg_attr(feature = "codec", serde(borrow))] Ident<'a>);

impl<'a> ConstantName<'a> {
    /// Init `ConstantName`, especially useful for testing
    #[must_use]
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for ConstantName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl GetName for ConstantName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

/// `FunctionName` function name for `Function` elements of AST.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionName<'a>(#[cfg_attr(feature = "codec", serde(borrow))] Ident<'a>);

impl<'a> FunctionName<'a> {
    #[must_use]
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for FunctionName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl<'a> std::fmt::Display for FunctionName<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.fragment())
    }
}

/// `ParameterName` parameter name element of AST, used for `Function`
/// parameters declaration.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ParameterName<'a>(#[cfg_attr(feature = "codec", serde(borrow))] Ident<'a>);

impl<'a> ParameterName<'a> {
    #[must_use]
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl std::fmt::Display for ParameterName<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0.fragment())
    }
}

/// `ValueName` value name element of AST. It's basic entity for:
/// - `Struct` type declaration
/// - `LetBinding` declaration
/// - `Binding` declaration
/// - `ExpressionValue` declaration
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ValueName<'a>(#[cfg_attr(feature = "codec", serde(borrow))] Ident<'a>);

impl<'a> ValueName<'a> {
    #[must_use]
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for ValueName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl GetName for ValueName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

/// `CodeLocation` code location of source for AST elements.
/// Contains: `line` nad `position`.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct CodeLocation(u32, usize);

impl CodeLocation {
    /// Initialize code location with:
    /// - `location` - line of source code
    /// - `offset` - position on the line of source code
    #[must_use]
    pub const fn new(location: u32, offset: usize) -> Self {
        Self(location, offset)
    }

    /// Get location line in the source
    #[must_use]
    pub const fn line(&self) -> u32 {
        self.0
    }

    /// Get location position on the line in the source
    #[must_use]
    pub const fn offset(&self) -> usize {
        self.1
    }
}

/// `PrimitiveTypes` primitive types elements of AST.
/// It's represent basic (primitive) types.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum PrimitiveTypes {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
    Bool,
    Char,
    String,
    Ptr,
    None,
}

impl GetName for PrimitiveTypes {
    fn name(&self) -> String {
        match self {
            Self::U8 => "u8".to_string(),
            Self::U16 => "u16".to_string(),
            Self::U32 => "u32".to_string(),
            Self::U64 => "u64".to_string(),
            Self::I8 => "i8".to_string(),
            Self::I16 => "i16".to_string(),
            Self::I32 => "i32".to_string(),
            Self::I64 => "i64".to_string(),
            Self::F32 => "f32".to_string(),
            Self::F64 => "f64".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Char => "char".to_string(),
            Self::String => "str".to_string(),
            Self::Ptr => "ptr".to_string(),
            Self::None => "()".to_string(),
        }
    }
}

/// `ExpressionStructValue` expression struct value element of AST.
/// Used for expression value declaration. The basic entity is:
/// - value name of struct type
/// - value struct type attribute
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExpressionStructValue<'a> {
    /// Value name of struct typed value
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: ValueName<'a>,
    /// Attribute name of struct typed value
    pub attribute: ValueName<'a>,
}

/// `StructType` struct type basic element used for `StructTypes`.
/// It contains basic elements:
/// - attribute name
/// - attribute type
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct StructType<'a> {
    /// Attribute name entity of struct type
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub attr_name: Ident<'a>,
    /// Attribute type entity of struct type
    pub attr_type: Type<'a>,
}

impl GetName for StructType<'_> {
    fn name(&self) -> String {
        (*self.attr_name.fragment()).to_string()
    }
}

/// `StructTypes` struct type element of AST.
/// Basic entity to declare struct complex types and its attributes.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct StructTypes<'a> {
    /// Struct type name
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: Ident<'a>,
    /// Struct type attributes
    pub attributes: Vec<StructType<'a>>,
}

impl GetLocation for StructTypes<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.name.location_line(), self.name.location_offset())
    }
}

impl<'a> GetName for StructTypes<'a> {
    fn name(&self) -> String {
        (*self.name.fragment()).to_string()
    }
}

/// `Type` element of AST.
/// Basic entity that represents types. Basic type entity is:
/// - Primitive types
/// - Struct types
/// - Arrays
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum Type<'a> {
    Primitive(PrimitiveTypes),
    #[cfg_attr(feature = "codec", serde(borrow))]
    Struct(StructTypes<'a>),
    Array(Box<Self>, u32),
}

impl<'a> GetName for Type<'a> {
    fn name(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.name(),
            Self::Struct(struct_type) => (*struct_type.name.fragment()).to_string(),
            Self::Array(array_type, size) => {
                format!("[{:?};{:?}]", array_type.name(), size)
            }
        }
    }
}

/// `ConstantValue` constant value element of AST.
/// Used for `ConstantExpression`. Constant value basic entities:
/// - `constant` it can contain other constant
/// - `value` - primitive value (like numbers etc.)
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ConstantValue<'a> {
    #[cfg_attr(feature = "codec", serde(borrow))]
    Constant(ConstantName<'a>),
    Value(PrimitiveValue),
}

/// `ConstantExpression` constant expression element of AST.
/// Used to declare constants.
/// The constant expression value based on other constant or primitive values.
/// Constant Expression can contain optional expression operation with other
/// constant expression. So it can be represented as Constant expression
/// tree as operations with other constant expressions.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ConstantExpression<'a> {
    /// Constant value - can be other constant 0r primitive value
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub value: ConstantValue<'a>,
    /// Constant expression optional expression operation with other constant expression declarations.
    pub operation: Option<(ExpressionOperations, Box<ConstantExpression<'a>>)>,
}

/// `Constant` constant declaration element of AST.
/// Basic constant entity contains:
/// - constant name
/// - constant type
/// - constant value - based on constant expression
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Constant<'a> {
    /// Constant name
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: ConstantName<'a>,
    /// Constant type
    pub constant_type: Type<'a>,
    /// Constant value based on constant expression
    pub constant_value: ConstantExpression<'a>,
}

impl GetLocation for Constant<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for Constant<'_> {
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
    }
}

/// `FunctionParameter` function parameter element of AST.
/// Used for `FunctionStatement` declaration.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionParameter<'a> {
    /// Function parameter name
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: ParameterName<'a>,
    /// Function parameter type
    pub parameter_type: Type<'a>,
}

/// `FunctionStatement` it's one of the most basic element of AST.
/// Basic entity of program logic. It contains function declaration and
/// function body.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Function name
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: FunctionName<'a>,
    /// Function parameters
    pub parameters: Vec<FunctionParameter<'a>>,
    /// Function result type
    pub result_type: Type<'a>,
    /// Function body
    pub body: Vec<BodyStatement<'a, I, E>>,
    _marker: PhantomData<I>,
}

impl<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> FunctionStatement<'a, I, E> {
    #[must_use]
    pub fn new(
        name: FunctionName<'a>,
        parameters: Vec<FunctionParameter<'a>>,
        result_type: Type<'a>,
        body: Vec<BodyStatement<'a, I, E>>,
    ) -> Self {
        Self {
            name,
            parameters,
            result_type,
            body,
            _marker: PhantomData,
        }
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation
    for FunctionStatement<'_, I, E>
{
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetName
    for FunctionStatement<'_, I, E>
{
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
    }
}

/// `PrimitiveValue` represents primitive value element of AST.
/// Values based on primitive types.
/// Used for `ConstantValue` and `ExpressionValue`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum PrimitiveValue {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
    Bool(bool),
    String(String),
    Char(char),
    Ptr,
    None,
}

impl PrimitiveValue {
    #[must_use]
    pub const fn get_type(&self) -> Type<'_> {
        match self {
            Self::U8(_) => Type::Primitive(PrimitiveTypes::U8),
            Self::U16(_) => Type::Primitive(PrimitiveTypes::U16),
            Self::U32(_) => Type::Primitive(PrimitiveTypes::U32),
            Self::U64(_) => Type::Primitive(PrimitiveTypes::U64),
            Self::I8(_) => Type::Primitive(PrimitiveTypes::I8),
            Self::I16(_) => Type::Primitive(PrimitiveTypes::I16),
            Self::I32(_) => Type::Primitive(PrimitiveTypes::I32),
            Self::I64(_) => Type::Primitive(PrimitiveTypes::I64),
            Self::F32(_) => Type::Primitive(PrimitiveTypes::F32),
            Self::F64(_) => Type::Primitive(PrimitiveTypes::F64),
            Self::Char(_) => Type::Primitive(PrimitiveTypes::Char),
            Self::Bool(_) => Type::Primitive(PrimitiveTypes::Bool),
            Self::String(_) => Type::Primitive(PrimitiveTypes::String),
            Self::Ptr => Type::Primitive(PrimitiveTypes::Ptr),
            Self::None => Type::Primitive(PrimitiveTypes::None),
        }
    }
}

/// `ExpressionValue` expression value element of AST.
/// Basic entity for `Expression` elements of AST.
/// Expression value contains entities:
/// - `ValueName` - value name of expression
/// - `PrimitiveValue` - primitive value name of expression (for
///   example, it's numbers: 10, 3.2 and other primitive values)
/// - `FunctionCall` - function call (with parameters) of expression
/// - `StructValue` - value of expression based on `Struct` types.
/// - `Expression` - expression representation (sub branch)
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ExpressionValue<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Value name of expression
    #[cfg_attr(feature = "codec", serde(borrow))]
    ValueName(ValueName<'a>),
    /// Primitive value of expression (like numbers etc.)
    PrimitiveValue(PrimitiveValue),
    /// Function call (with parameters) of expression
    FunctionCall(FunctionCall<'a, I, E>),
    /// Value of expression based on `Struct` types.
    StructValue(ExpressionStructValue<'a>),
    /// Expression representation (sub branch)
    Expression(Box<Expression<'a, I, E>>),
    /// Extended expression
    ExtendedExpression(Box<E>),
    #[cfg_attr(feature = "codec", serde(skip))]
    _Marker(Infallible, PhantomData<I>),
}

/// `ExpressionOperations` expression operation element of AST.
/// Used for expression operations:
/// - `ConstantExpression` - expression of constant declaration
/// - `Expression` - part of operations for expressions
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ExpressionOperations {
    Plus,
    Minus,
    Multiply,
    Divide,
    ShiftLeft,
    ShiftRight,
    And,
    Or,
    Xor,
    Eq,
    NotEq,
    Great,
    Less,
    GreatEq,
    LessEq,
}

impl ExpressionOperations {
    /// Get expression operation priority level
    #[must_use]
    pub const fn priority(&self) -> u8 {
        match self {
            Self::Plus => 5,
            Self::Minus => 4,
            Self::Divide => 8,
            Self::Multiply | Self::ShiftLeft | Self::ShiftRight => {
                MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
            }
            Self::Or | Self::Xor => 6,
            Self::And
            | Self::Eq
            | Self::NotEq
            | Self::Great
            | Self::Less
            | Self::GreatEq
            | Self::LessEq => 7,
        }
    }
}

/// `Expression` element of AST is basic entity that
/// represent expression, and optionally expression with optional
/// operations with other expression. So it can be expression tree
/// with expression operations.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Expression<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Expression value itself
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub expression_value: ExpressionValue<'a, I, E>,
    /// Optional expression operation with other expression value
    pub operation: Option<(ExpressionOperations, Box<Expression<'a, I, E>>)>,
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation for Expression<'_, I, E> {
    fn location(&self) -> CodeLocation {
        // TODO: extend it
        CodeLocation::new(1, 0)
    }
}

/// `LetBinding` let binding element of AST. Basic entity
/// for `values` declarations.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize), serde(tag = "type"))]
pub struct LetBinding<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Value name of let binding
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: ValueName<'a>,
    /// Mutability flag of binding
    pub mutable: bool,
    /// Optional type of value
    pub value_type: Option<Type<'a>>,
    /// Value expression to bind as result of let bending
    pub value: Box<Expression<'a, I, E>>,
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation for LetBinding<'_, I, E> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetName for LetBinding<'_, I, E> {
    fn name(&self) -> String {
        self.name.0.to_string()
    }
}

/// `Binding` binding element of AST. Basic entity
/// for `values` re-declaration, to bind new values for already
/// declared values.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Binding<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Binding value name
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: ValueName<'a>,
    /// Value expression as result of binding
    pub value: Box<Expression<'a, I, E>>,
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation for Binding<'_, I, E> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetName for Binding<'_, I, E> {
    fn name(&self) -> String {
        self.name.0.to_string()
    }
}

/// `FunctionCall` function call element of AST.
/// Basic entity for function call representation.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionCall<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Function name of called function
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub name: FunctionName<'a>,
    /// Function parameters, represented through expression
    pub parameters: Vec<Expression<'a, I, E>>,
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation
    for FunctionCall<'_, I, E>
{
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetName for FunctionCall<'_, I, E> {
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
    }
}

/// `Condition` condition declarations.
/// Used for `ExpressionCondition`. Contains basic condition
/// entities: `<. >, ==, <=, >=, !=`
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum Condition {
    Great,
    Less,
    Eq,
    GreatEq,
    LessEq,
    NotEq,
}

/// `LogicCondition` declaration of logical condition operation.
/// It can contains only: `and`, `or`. Used for `IfCondition` element of AST.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum LogicCondition {
    And,
    Or,
}

/// `ExpressionCondition` expression condition element of AST.
/// Used in `ExpressionLogicCondition` for `IfCondition` declaration.
/// It contains condition between twe expressions.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExpressionCondition<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Left expression
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub left: Expression<'a, I, E>,
    /// Condition between left and right expressions
    pub condition: Condition,
    /// Right expression
    pub right: Expression<'a, I, E>,
}

/// # Logic expression condition
/// `ExpressionLogicCondition` expression logic condition used for
/// `IfCondition` declaration.
/// Expression logic condition can contains left and optional right
/// parts. Right part represent logic condition (like `or`, `and`) to
/// other `ExpressionLogicCondition`. So finally ir can represent tree
/// expressions logic conditions.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExpressionLogicCondition<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Left expression condition
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub left: ExpressionCondition<'a, I, E>,
    /// Optional right side contain logic operation to other `ExpressionLogicCondition`
    pub right: Option<(LogicCondition, Box<ExpressionLogicCondition<'a, I, E>>)>,
}

/// `IfCondition` if-condition control flow element of AST.
/// One of the basic control flow. Basic entities:
/// - single expression condition
/// - logic expression condition tree
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum IfCondition<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Single if condition based on expression
    #[cfg_attr(feature = "codec", serde(borrow))]
    Single(Expression<'a, I, E>),
    /// Logic expression condition tree
    Logic(ExpressionLogicCondition<'a, I, E>),
}

/// `IfStatement` if statement AST element.
/// Contains full representation of if statement with entities:
/// - if-condition
/// - If-body statement - body of if-condition success
/// - Ff-else-body statement - body of else-condition success
/// - Else-if-body statement - body of else-if-condition success
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct IfStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// If-condition
    #[cfg_attr(feature = "codec", serde(borrow))]
    pub condition: IfCondition<'a, I, E>,
    /// If-body statement - body of if-condition success
    pub body: IfBodyStatements<'a, I, E>,
    /// If-else-body statement - body of else-condition success
    pub else_statement: Option<IfBodyStatements<'a, I, E>>,
    /// Else-if-body statement - body of else-if-condition success
    pub else_if_statement: Option<Box<IfStatement<'a, I, E>>>,
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> GetLocation
    for IfStatement<'_, I, E>
{
    fn location(&self) -> CodeLocation {
        // TODO
        CodeLocation::new(1, 0)
    }
}

/// `BodyStatement` one of the basic AST elements.
/// It's part of Function body.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum BodyStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Let-binding function declaration
    #[cfg_attr(feature = "codec", serde(borrow))]
    LetBinding(LetBinding<'a, I, E>),
    /// Binding function declaration
    Binding(Binding<'a, I, E>),
    /// Function call
    FunctionCall(FunctionCall<'a, I, E>),
    /// If-condition control flow statement
    If(IfStatement<'a, I, E>),
    /// Loop control flow statement
    Loop(Vec<LoopBodyStatement<'a, I, E>>),
    /// Expression statement
    Expression(Expression<'a, I, E>),
    /// Return statement
    Return(Expression<'a, I, E>),
}

/// `IfBodyStatement` statement of if-body elements tree of AST.
/// Used as body statement of If-control flow.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum IfBodyStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    #[cfg_attr(feature = "codec", serde(borrow))]
    LetBinding(LetBinding<'a, I, E>),
    Binding(Binding<'a, I, E>),
    FunctionCall(FunctionCall<'a, I, E>),
    If(IfStatement<'a, I, E>),
    Loop(Vec<LoopBodyStatement<'a, I, E>>),
    Return(Expression<'a, I, E>),
}

/// `IfLoopBodyStatement` statement of loop-if-body elements tree of AST.
/// Used as body statement of If-control flow in the `Loop` AST element.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
#[cfg_attr(feature = "codec", serde(tag = "type", content = "content"))]
pub enum IfLoopBodyStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    #[cfg_attr(feature = "codec", serde(borrow))]
    LetBinding(LetBinding<'a, I, E>),
    Binding(Binding<'a, I, E>),
    FunctionCall(FunctionCall<'a, I, E>),
    If(IfStatement<'a, I, E>),
    Loop(Vec<LoopBodyStatement<'a, I, E>>),
    Return(Expression<'a, I, E>),
    Break,
    Continue,
}

/// `IfBodyStatements` set of elements in the AST, that represents
/// control flow: `if`, `loop`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum IfBodyStatements<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    #[cfg_attr(feature = "codec", serde(borrow))]
    If(Vec<IfBodyStatement<'a, I, E>>),
    Loop(Vec<IfLoopBodyStatement<'a, I, E>>),
}

/// `LoopBodyStatement` statement of loop-body elements tree of AST.
/// Used as body statement of loop-control flow.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum LoopBodyStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    #[cfg_attr(feature = "codec", serde(borrow))]
    LetBinding(LetBinding<'a, I, E>),
    Binding(Binding<'a, I, E>),
    FunctionCall(FunctionCall<'a, I, E>),
    If(IfStatement<'a, I, E>),
    Loop(Vec<LoopBodyStatement<'a, I, E>>),
    Return(Expression<'a, I, E>),
    Break,
    Continue,
}

/// `MainStatement` main AST statement for all elements.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum MainStatement<'a, I: SemanticContextInstruction, E: ExtendedExpression<I>> {
    /// Import declarations
    #[cfg_attr(feature = "codec", serde(borrow))]
    Import(ImportPath<'a>),
    /// Constant declarations
    Constant(Constant<'a>),
    /// Type declaration
    Types(StructTypes<'a>),
    /// Function declaration and function body-statement
    Function(FunctionStatement<'a, I, E>),
}

/// # Main
/// Stack of `MainStatement` main AST elements. That gather
/// tries of AST, to represent full sort of source code.
pub type Main<'a, I, E> = Vec<MainStatement<'a, I, E>>;
