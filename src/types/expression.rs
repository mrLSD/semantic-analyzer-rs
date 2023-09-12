use crate::ast;
use crate::types::types::Type;
use crate::types::{FunctionCall, PrimitiveValue, StructValue, ValueName};

/// # Expression result
/// Contains analyzing results of expression:
/// - `expr_type` - result type of expression
/// - `expr_value` - result value of expression
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionResult {
    pub expr_type: Type,
    pub expr_value: ExpressionResultValue,
}

/// # Expression Result Value
/// Result value of expression analyze has to kind:
/// - Primitive value
/// - Register that contain result of expression
///   evaluation or call.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionResultValue {
    PrimitiveValue(PrimitiveValue),
    Register,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue {
    ValueName(ValueName),
    PrimitiveValue(PrimitiveValue),
    StructValue(StructValue),
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

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expression_value: ExpressionValue,
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
