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

/// Value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
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

impl ToString for ValueName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Inner value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression {
    pub value: ConstantValue,
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
pub struct Constant {
    pub name: ConstantName,
    pub constant_type: Type,
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
pub struct Value {
    pub inner_name: InnerValueName,
    pub inner_type: Type,
    pub mutable: bool,
    pub alloca: bool,
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
pub struct Function {
    pub inner_name: FunctionName,
    pub inner_type: Type,
    pub parameters: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterName(String);

impl From<ast::ParameterName<'_>> for ParameterName {
    fn from(value: ast::ParameterName<'_>) -> Self {
        Self(value.to_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionParameter {
    pub name: ParameterName,
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

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStatement {
    pub name: FunctionName,
    pub parameters: Vec<FunctionParameter>,
    pub result_type: Type,
    pub body: Vec<BodyStatement>,
}

impl From<ast::FunctionStatement<'_>> for FunctionStatement {
    fn from(value: ast::FunctionStatement<'_>) -> Self {
        Self {
            name: value.name.into(),
            parameters: value.parameters.iter().map(|v| v.clone().into()).collect(),
            result_type: value.result_type.into(),
            body: value.body.iter().map(|v| v.clone().into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Expression(Expression),
    Return(Expression),
}

impl From<ast::BodyStatement<'_>> for BodyStatement {
    fn from(value: ast::BodyStatement<'_>) -> Self {
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

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub name: ValueName,
    pub mutable: bool,
    pub value_type: Option<Type>,
    pub value: Box<Expression>,
}

impl ToString for LetBinding {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl From<ast::LetBinding<'_>> for LetBinding {
    fn from(value: ast::LetBinding<'_>) -> Self {
        Self {
            name: value.name.into(),
            mutable: value.mutable,
            value_type: value.value_type.map(Into::into),
            value: Box::new(value.value.as_ref().clone().into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
            Self::Char(c) => format!("{}", c),
            Self::Ptr => "ptr".to_string(),
            Self::None => "None".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValue {
    pub name: ValueName,
    pub attribute: ValueName,
}

impl ToString for StructValue {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl From<ast::StructValue<'_>> for StructValue {
    fn from(value: ast::StructValue<'_>) -> Self {
        Self {
            name: value.name.into(),
            attribute: value.attribute.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: FunctionName,
    pub parameters: Vec<Expression>,
}

impl ToString for FunctionCall {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl From<ast::FunctionCall<'_>> for FunctionCall {
    fn from(value: ast::FunctionCall<'_>) -> Self {
        Self {
            name: value.name.into(),
            parameters: value.parameters.iter().map(|v| v.clone().into()).collect(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub name: ValueName,
    pub value: Box<Expression>,
}

impl ToString for Binding {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl From<ast::Binding<'_>> for Binding {
    fn from(value: ast::Binding<'_>) -> Self {
        Self {
            name: value.name.into(),
            value: Box::new(value.value.as_ref().clone().into()),
        }
    }
}
