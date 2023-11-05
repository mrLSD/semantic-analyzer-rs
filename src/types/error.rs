//! # Errors types
//! Errors types for Semantic analyzer result of Error state.

use crate::ast::CodeLocation;

/// Common errors kind for the State.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StateErrorKind {
    /// Common error indicate errors in the State
    Common,
    ConstantAlreadyExist,
    ConstantNotFound,
    WrongLetType,
    WrongExpressionType,
    TypeAlreadyExist,
    FunctionAlreadyExist,
    ValueNotFound,
    ValueNotStruct,
    ValueNotStructField,
    ValueIsNotMutable,
    FunctionNotFound,
    FunctionParameterTypeWrong,
    ReturnNotFound,
    ReturnAlreadyCalled,
    IfElseDuplicated,
    TypeNotFound,
    WrongReturnType,
    ConditionExpressionWrongType,
    ConditionExpressionNotSupported,
}

/// State error location. Useful to determine location of error
#[derive(Debug, Clone)]
pub struct StateErrorLocation(pub CodeLocation);

/// State error result data representation
#[derive(Debug, Clone)]
pub struct StateErrorResult {
    /// Kind of error
    pub kind: StateErrorKind,
    /// Error value
    pub value: String,
    /// Error location
    pub location: StateErrorLocation,
}

impl StateErrorResult {
    pub const fn new(kind: StateErrorKind, value: String, location: CodeLocation) -> Self {
        Self {
            kind,
            value,
            location: StateErrorLocation(location),
        }
    }
}

impl StateErrorResult {
    #[allow(dead_code)]
    /// Get state trace data from error result as string
    pub fn trace_state(&self) -> String {
        format!(
            "[{:?}] for value {:?} at: {:?}:{:?}",
            self.kind,
            self.value,
            self.location.0.line(),
            self.location.0.offset()
        )
    }
}
