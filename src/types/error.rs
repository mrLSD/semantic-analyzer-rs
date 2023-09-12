
use crate::ast::CodeLocation;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum StateErrorKind {
    /// Common error indicate errors in the State
    Common,
    ConstantAlreadyExist,
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
    IfElseDuplicated,
    TypeNotFound,
    WrongReturnType,
    ConditionExpressionWrongType,
    ConditionExpressionNotSupported,
}

#[derive(Debug, Clone)]
pub struct StateErrorLocation(pub CodeLocation);

#[derive(Debug, Clone)]
pub struct StateErrorResult {
    pub kind: StateErrorKind,
    pub value: String,
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
