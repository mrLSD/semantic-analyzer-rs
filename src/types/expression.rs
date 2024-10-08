//! # Expression types
//! Expression types for Semantic analyzer result state.

use super::types::Type;
use super::{FunctionCall, PrimitiveValue, ValueName};
use crate::ast;
use crate::types::semantic::{ExtendedExpression, SemanticContextInstruction};
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::fmt::Display;

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

/// `ExtendedExpressionValue` represent simplified string
/// data of custom extended `ExpressionValue`. For now
/// used only for display errors.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct ExtendedExpressionValue(String);

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
    ExtendedExpression(ExtendedExpressionValue),
}

impl Display for ExpressionValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::ValueName(val) => val.clone().to_string(),
            Self::PrimitiveValue(val) => val.clone().to_string(),
            Self::StructValue(st_val) => st_val.clone().to_string(),
            Self::FunctionCall(fn_call) => fn_call.clone().to_string(),
            Self::Expression(val) => val.to_string(),
            Self::ExtendedExpression(val) => val.clone().0,
        };
        write!(f, "{str}")
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> From<ast::ExpressionValue<'_, I, E>>
    for ExpressionValue
{
    fn from(value: ast::ExpressionValue<'_, I, E>) -> Self {
        match value {
            #[allow(unreachable_patterns)]
            ast::ExpressionValue::_marker(..) => unreachable!(),
            ast::ExpressionValue::ValueName(v) => Self::ValueName(v.into()),
            ast::ExpressionValue::PrimitiveValue(v) => Self::PrimitiveValue(v.into()),
            ast::ExpressionValue::StructValue(v) => Self::StructValue(v.into()),
            ast::ExpressionValue::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::ExpressionValue::Expression(v) => {
                Self::Expression(Box::new(v.as_ref().clone().into()))
            }
            ast::ExpressionValue::ExtendedExpression(expr) => {
                Self::ExtendedExpression(ExtendedExpressionValue(format!("{expr:?}")))
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

impl Display for ExpressionStructValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
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

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.expression_value)
    }
}

impl<I: SemanticContextInstruction, E: ExtendedExpression<I>> From<ast::Expression<'_, I, E>>
    for Expression
{
    fn from(value: ast::Expression<'_, I, E>) -> Self {
        Self {
            expression_value: value.expression_value.into(),
            operation: value
                .operation
                .map(|(op, expr)| (op.into(), Box::new(expr.as_ref().clone().into()))),
        }
    }
}
