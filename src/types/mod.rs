//! # Types
//! Semantic analyzer type.
//! Contains basic entities:
//! - Semantic types system
//! - Semantic basic elements types
//! - Block state types
//! - Error types

#![allow(clippy::module_inception)]
/// Block state types
pub mod block_state;
/// Condition types
pub mod condition;
/// Error types
pub mod error;
/// Expression types
pub mod expression;
/// Basic semantic types
pub mod semantic;
/// Types for type system
pub mod types;

use self::condition::{IfStatement, LoopBodyStatement};
use self::expression::{Expression, ExpressionOperations};
use self::types::Type;
use crate::ast;
use crate::ast::GetName;
use crate::types::semantic::ExtendedExpression;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};

/// Value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ValueName(String);

impl From<ast::ValueName<'_>> for ValueName {
    fn from(value: ast::ValueName<'_>) -> Self {
        Self(value.name())
    }
}

impl From<String> for ValueName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for ValueName {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl ToString for ValueName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Inner value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct InnerValueName(String);

impl From<ValueName> for InnerValueName {
    fn from(value: ValueName) -> Self {
        Self(value.0)
    }
}

impl From<String> for InnerValueName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl From<&str> for InnerValueName {
    fn from(value: &str) -> Self {
        Self(value.to_string())
    }
}

impl ToString for InnerValueName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Label name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct LabelName(String);

impl From<String> for LabelName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl ToString for LabelName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Function name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionName(String);

impl From<String> for FunctionName {
    fn from(value: String) -> Self {
        Self(value)
    }
}
impl From<ast::FunctionName<'_>> for FunctionName {
    fn from(value: ast::FunctionName<'_>) -> Self {
        Self(value.to_string())
    }
}

impl ToString for FunctionName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Constant name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ConstantName(String);

impl From<ast::ConstantName<'_>> for ConstantName {
    fn from(value: ast::ConstantName<'_>) -> Self {
        Self(value.name())
    }
}

impl From<String> for ConstantName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl ToString for ConstantName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Constant value can contain other constant or primitive value
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ConstantValue {
    Constant(ConstantName),
    Value(PrimitiveValue),
}

impl From<ast::ConstantValue<'_>> for ConstantValue {
    fn from(value: ast::ConstantValue<'_>) -> Self {
        match value {
            ast::ConstantValue::Constant(v) => Self::Constant(v.into()),
            ast::ConstantValue::Value(v) => Self::Value(v.into()),
        }
    }
}

/// Constant expression represent expression operation between
/// constant values, and represent flat tree
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ConstantExpression {
    /// Constant value for expression operation
    pub value: ConstantValue,
    /// Optional expression operation and next constant expression entry point
    pub operation: Option<(ExpressionOperations, Box<ConstantExpression>)>,
}

impl From<ast::ConstantExpression<'_>> for ConstantExpression {
    fn from(value: ast::ConstantExpression<'_>) -> Self {
        Self {
            value: value.value.into(),
            operation: value
                .operation
                .map(|(op, expr)| (op.into(), Box::new(expr.as_ref().clone().into()))),
        }
    }
}

/// # Constant
/// Can contain: name, type
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Constant {
    /// Constant name
    pub name: ConstantName,
    /// Constant type
    pub constant_type: Type,
    /// Constant value represented through constant expression
    pub constant_value: ConstantExpression,
}

impl From<ast::Constant<'_>> for Constant {
    fn from(value: ast::Constant<'_>) -> Self {
        Self {
            name: value.name.into(),
            constant_type: value.constant_type.into(),
            constant_value: value.constant_value.into(),
        }
    }
}

/// # Values
/// Can contain inner data: name, type, memory allocation status:
/// - alloca - stack allocation
/// - malloc - malloc allocation
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Value {
    /// Inner value name
    pub inner_name: InnerValueName,
    /// Inner value type
    pub inner_type: Type,
    /// Mutability flag
    pub mutable: bool,
    /// Stack allocation flag
    pub alloca: bool,
    /// Memory allocation flag
    pub malloc: bool,
}

/// # Function
/// Function declaration analyze contains:
/// - function name
/// - function type
/// - parameters of functions (with types only)
///
/// It used to detect functions in state and
/// their parameters to use in normal execution
/// flog.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Function {
    /// Inner function name
    pub inner_name: FunctionName,
    /// Inner (return) type
    pub inner_type: Type,
    /// Function parameters typesz
    pub parameters: Vec<Type>,
}

/// Parameter name type for Functions parameter
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ParameterName(String);

impl From<ast::ParameterName<'_>> for ParameterName {
    fn from(value: ast::ParameterName<'_>) -> Self {
        Self(value.to_string())
    }
}

/// Function parameter one of the basic entity for `FunctionStatement`
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionParameter {
    /// Function parameter name
    pub name: ParameterName,
    /// Function parameter type
    pub parameter_type: Type,
}

impl From<ast::FunctionParameter<'_>> for FunctionParameter {
    fn from(value: ast::FunctionParameter<'_>) -> Self {
        Self {
            name: value.name.into(),
            parameter_type: value.parameter_type.into(),
        }
    }
}

impl ToString for FunctionParameter {
    fn to_string(&self) -> String {
        self.name.0.clone()
    }
}

/// # Function statement
/// Function statement is basic type that represent function itself.
/// The basic function struct elements:
/// - function name
/// - function parameters
/// - function result type
/// - function body statements
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionStatement {
    /// Function name
    pub name: FunctionName,
    /// Function parameters
    pub parameters: Vec<FunctionParameter>,
    /// Function result type
    pub result_type: Type,
    /// Function body statements
    pub body: Vec<BodyStatement>,
}

impl<E: ExtendedExpression> From<ast::FunctionStatement<'_, E>> for FunctionStatement {
    fn from(value: ast::FunctionStatement<'_, E>) -> Self {
        Self {
            name: value.name.into(),
            parameters: value.parameters.iter().map(|v| v.clone().into()).collect(),
            result_type: value.result_type.into(),
            body: value.body.iter().map(|v| v.clone().into()).collect(),
        }
    }
}

/// # Body statement
/// Statement of body. Body is basic entity for functions and
/// represent basic functions elements.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum BodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Expression(Expression),
    Return(Expression),
}

impl<E: ExtendedExpression> From<ast::BodyStatement<'_, E>> for BodyStatement {
    fn from(value: ast::BodyStatement<'_, E>) -> Self {
        match value {
            ast::BodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::BodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::BodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::BodyStatement::If(v) => Self::If(v.into()),
            ast::BodyStatement::Loop(v) => Self::Loop(v.iter().map(|v| v.clone().into()).collect()),
            ast::BodyStatement::Expression(v) => Self::Expression(v.into()),
            ast::BodyStatement::Return(v) => Self::Return(v.into()),
        }
    }
}

/// # Let binding
/// Value initialization through binding.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct LetBinding {
    /// Value name
    pub name: ValueName,
    /// Value mutability flag
    pub mutable: bool,
    /// Value type
    pub value_type: Option<Type>,
    /// Value bind expression
    pub value: Box<Expression>,
}

impl ToString for LetBinding {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl<E: ExtendedExpression> From<ast::LetBinding<'_, E>> for LetBinding {
    fn from(value: ast::LetBinding<'_, E>) -> Self {
        Self {
            name: value.name.into(),
            mutable: value.mutable,
            value_type: value.value_type.map(Into::into),
            value: Box::new(value.value.as_ref().clone().into()),
        }
    }
}

/// Primitive value is most primitive and basic values entity.
/// It's basic elements for all other values elements.
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

impl From<ast::PrimitiveValue> for PrimitiveValue {
    fn from(value: ast::PrimitiveValue) -> Self {
        match value {
            ast::PrimitiveValue::U8(v) => Self::U8(v),
            ast::PrimitiveValue::U16(v) => Self::U16(v),
            ast::PrimitiveValue::U32(v) => Self::U32(v),
            ast::PrimitiveValue::U64(v) => Self::U64(v),
            ast::PrimitiveValue::I8(v) => Self::I8(v),
            ast::PrimitiveValue::I16(v) => Self::I16(v),
            ast::PrimitiveValue::I32(v) => Self::I32(v),
            ast::PrimitiveValue::I64(v) => Self::I64(v),
            ast::PrimitiveValue::F32(v) => Self::F32(v),
            ast::PrimitiveValue::F64(v) => Self::F64(v),
            ast::PrimitiveValue::Bool(v) => Self::Bool(v),
            ast::PrimitiveValue::String(v) => Self::String(v),
            ast::PrimitiveValue::Char(v) => Self::Char(v),
            ast::PrimitiveValue::Ptr => Self::Ptr,
            ast::PrimitiveValue::None => Self::None,
        }
    }
}

impl ToString for PrimitiveValue {
    fn to_string(&self) -> String {
        match self {
            Self::U8(val) => val.clone().to_string(),
            Self::U16(val) => val.clone().to_string(),
            Self::U32(val) => val.clone().to_string(),
            Self::U64(val) => val.clone().to_string(),
            Self::I8(val) => val.clone().to_string(),
            Self::I16(val) => val.clone().to_string(),
            Self::I32(val) => val.clone().to_string(),
            Self::I64(val) => val.clone().to_string(),
            Self::F32(val) => val.clone().to_string(),
            Self::F64(val) => val.clone().to_string(),
            Self::Bool(val) => val.to_string(),
            Self::String(s) => s.clone(),
            Self::Char(c) => format!("{c}"),
            Self::Ptr => "ptr".to_string(),
            Self::None => "None".to_string(),
        }
    }
}

/// # Function call
/// Basic struct for function call representation
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct FunctionCall {
    /// Call function name
    pub name: FunctionName,
    /// Call function parameters contains expressions
    pub parameters: Vec<Expression>,
}

impl ToString for FunctionCall {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl<E: ExtendedExpression> From<ast::FunctionCall<'_, E>> for FunctionCall {
    fn from(value: ast::FunctionCall<'_, E>) -> Self {
        Self {
            name: value.name.into(),
            parameters: value.parameters.iter().map(|v| v.clone().into()).collect(),
        }
    }
}

/// `Binding` represents mutable binding for previously bind values
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Binding {
    /// Binding value name
    pub name: ValueName,
    /// Value expression representation
    pub value: Box<Expression>,
}

impl ToString for Binding {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl<E: ExtendedExpression> From<ast::Binding<'_, E>> for Binding {
    fn from(value: ast::Binding<'_, E>) -> Self {
        Self {
            name: value.name.into(),
            value: Box::new(value.value.as_ref().clone().into()),
        }
    }
}
