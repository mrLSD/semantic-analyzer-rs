//! # Errors types
//! Errors types for Semantic analyzer result of Error state.

use crate::ast::CodeLocation;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};

/// Common errors kind for the State.
#[derive(Debug, Clone, Eq, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
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
    ConditionIsEmpty,
    ConditionExpressionNotSupported,
    ForbiddenCodeAfterReturnDeprecated,
    ForbiddenCodeAfterContinueDeprecated,
    ForbiddenCodeAfterBreakDeprecated,
    FunctionArgumentNameDuplicated,
}

/// State error location. Useful to determine location of error
#[derive(Debug, Clone)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct StateErrorLocation(pub CodeLocation);

/// State error result data representation
#[derive(Debug, Clone)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct StateErrorResult {
    /// Kind of error
    pub kind: StateErrorKind,
    /// Error value
    pub value: String,
    /// Error location
    pub location: StateErrorLocation,
}

impl StateErrorResult {
    #[must_use]
    pub const fn new(kind: StateErrorKind, value: String, location: CodeLocation) -> Self {
        Self {
            kind,
            value,
            location: StateErrorLocation(location),
        }
    }
}

impl StateErrorResult {
    /// Get state trace data from error result as string
    #[must_use]
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
