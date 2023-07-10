use crate::ast;

/// State result type - for single results
pub type StateResult<T> = Result<T, error::StateError>;

/// Value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ValueName(String);

impl From<String> for ValueName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

/// Inner value name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct InnerValueName(String);

impl From<String> for InnerValueName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl ToString for InnerValueName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Inner Type - type representation
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct InnerType(String);

impl From<String> for InnerType {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl ToString for InnerType {
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
impl ToString for FunctionName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

/// Constant name type
#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct ConstantName(String);

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

/// # Constant
/// Can contain: name, type
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Constant {
    pub name: ConstantName,
    pub inner_type: InnerType,
}

/// # Values
/// Can contain inner data: name, type, memory allocation status:
/// - alloca - stack allocation
/// - malloc - malloc allocation
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Value {
    pub inner_name: InnerValueName,
    pub inner_type: InnerType,
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
    pub inner_type: InnerType,
    pub parameters: Vec<InnerType>,
}

/// # Expression result
/// Contains analyzing results of expression:
/// - `expr_type` - result type of expression
/// - `expr_value` - result value of expression
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionResult {
    pub expr_type: InnerType,
    pub expr_value: ExpressionResultValue,
}

/// # Expression Result Value
/// Result value of expression analyze has to kind:
/// - Primitive value
/// - Register that contain result of expression
///   evaluation or call.
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionResultValue {
    PrimitiveValue(ast::PrimitiveValue),
    Register(u64),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionStatement;

#[allow(clippy::module_name_repetitions)]
pub mod error {
    #[derive(Debug, Clone)]
    pub struct EmptyError;

    #[derive(Debug, Clone)]
    pub struct StateError(pub StateErrorKind);

    #[derive(Debug, Clone)]
    pub enum StateErrorKind {
        /// Common error indicate errors in the State
        Common,
        ConstantAlreadyExist,
        WrongLetType,
        WrongExpressionType,
        TypeAlreadyExist,
        FunctionAlreadyExist,
        ValueNotFound,
        ValueIsNotMutable,
        FunctionNotFound,
        FunctionParameterTypeWrong,
        ReturnNotFound,
        IfElseDuplicated,
    }

    #[derive(Debug, Clone)]
    pub struct StateErrorLocation {
        pub line: u64,
        pub column: u64,
    }

    #[derive(Debug, Clone)]
    pub struct StateErrorResult {
        pub kind: StateErrorKind,
        pub value: String,
        pub location: StateErrorLocation,
    }

    impl StateErrorResult {
        pub const fn new(kind: StateErrorKind, value: String, line: u64, column: u64) -> Self {
            Self {
                kind,
                value,
                location: StateErrorLocation { line, column },
            }
        }
    }

    impl StateErrorResult {
        #[allow(dead_code)]
        pub fn trace_state(&self) -> String {
            format!(
                "[{:?}] for value {:?} at: {:?}:{:?}",
                self.kind, self.value, self.location.line, self.location.column
            )
        }
    }
}
