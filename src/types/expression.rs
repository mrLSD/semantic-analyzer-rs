//! # Expression types
//! Expression types for Semantic analyzer result state.

use super::types::Type;
use super::{FunctionCall, PrimitiveValue, ValueName};
use crate::ast;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};

/// # Expression result
/// Contains analyzing results of expression:
/// - `expr_type` - result type of expression
/// - `expr_value` - result value of expression
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExpressionResult {
    /// Result type of expression
    pub expr_type: Type,
    /// Result value of expression
    pub expr_value: ExpressionResultValue,
}

/// # Expression Result Value
/// Result value of expression analyze has to kind:
/// - Primitive value
/// - Register that contain result of expression
///   evaluation or call.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ExpressionResultValue {
    PrimitiveValue(PrimitiveValue),
    Register(u64),
}

/// Expression value kinds:
/// - value name - initialized through let-binding
/// - primitive value - most primitive value, like integers etc.
/// - struct value - value of struct type
/// - function call - call of function with params
/// - expression - contains other expression
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum ExpressionValue {
    ValueName(ValueName),
    PrimitiveValue(PrimitiveValue),
    StructValue(ExpressionStructValue),
    FunctionCall(FunctionCall),
    Expression(Box<Expression>),
}

impl ToString for ExpressionValue {
    fn to_string(&self) -> String {
        match self {
            Self::ValueName(val) => val.clone().to_string(),
            Self::PrimitiveValue(val) => val.clone().to_string(),
            Self::StructValue(st_val) => st_val.clone().to_string(),
            Self::FunctionCall(fn_call) => fn_call.clone().to_string(),
            Self::Expression(val) => val.to_string(),
        }
    }
}

impl From<ast::ExpressionValue<'_>> for ExpressionValue {
    fn from(value: ast::ExpressionValue<'_>) -> Self {
        match value {
            ast::ExpressionValue::ValueName(v) => Self::ValueName(v.into()),
            ast::ExpressionValue::PrimitiveValue(v) => Self::PrimitiveValue(v.into()),
            ast::ExpressionValue::StructValue(v) => Self::StructValue(v.into()),
            ast::ExpressionValue::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::ExpressionValue::Expression(v) => {
                Self::Expression(Box::new(v.as_ref().clone().into()))
            }
        }
    }
}

/// Expression value of struct type. It's represent access to
/// struct attributes of values with struct type
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExpressionStructValue {
    /// Value name for structure value
    pub name: ValueName,
    /// Value attribute for structure value
    pub attribute: ValueName,
}

impl ToString for ExpressionStructValue {
    fn to_string(&self) -> String {
        self.name.to_string()
    }
}

impl From<ast::ExpressionStructValue<'_>> for ExpressionStructValue {
    fn from(value: ast::ExpressionStructValue<'_>) -> Self {
        Self {
            name: value.name.into(),
            attribute: value.attribute.into(),
        }
    }
}

/// Basic  expression operations - calculations and
/// logic operations
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

impl From<ast::ExpressionOperations> for ExpressionOperations {
    fn from(value: ast::ExpressionOperations) -> Self {
        match value {
            ast::ExpressionOperations::Plus => Self::Plus,
            ast::ExpressionOperations::Minus => Self::Minus,
            ast::ExpressionOperations::Multiply => Self::Multiply,
            ast::ExpressionOperations::Divide => Self::Divide,
            ast::ExpressionOperations::ShiftLeft => Self::ShiftLeft,
            ast::ExpressionOperations::ShiftRight => Self::ShiftRight,
            ast::ExpressionOperations::And => Self::And,
            ast::ExpressionOperations::Or => Self::Or,
            ast::ExpressionOperations::Xor => Self::Xor,
            ast::ExpressionOperations::Eq => Self::Eq,
            ast::ExpressionOperations::NotEq => Self::NotEq,
            ast::ExpressionOperations::Great => Self::Great,
            ast::ExpressionOperations::Less => Self::Less,
            ast::ExpressionOperations::GreatEq => Self::GreatEq,
            ast::ExpressionOperations::LessEq => Self::LessEq,
        }
    }
}

/// # Expression
/// Basic expression entity representation. It contains
/// `ExpressionValue` and optional operations with other
/// expressions. So it's represent flat tree.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct Expression {
    /// Expression value
    pub expression_value: ExpressionValue,
    /// Optional expression operation under other `Expression`
    pub operation: Option<(ExpressionOperations, Box<Expression>)>,
}

impl ToString for Expression {
    fn to_string(&self) -> String {
        self.expression_value.to_string()
    }
}

impl From<ast::Expression<'_>> for Expression {
    fn from(value: ast::Expression<'_>) -> Self {
        Self {
            expression_value: value.expression_value.into(),
            operation: value
                .operation
                .map(|(op, expr)| (op.into(), Box::new(expr.as_ref().clone().into()))),
        }
    }
}
