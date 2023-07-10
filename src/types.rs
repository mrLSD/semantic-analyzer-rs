#![allow(dead_code, clippy::module_name_repetitions)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterName(String);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionParameter {
    pub name: ParameterName,
    pub parameter_type: Type,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionStatement {
    pub name: FunctionName,
    pub parameters: Vec<FunctionParameter>,
    pub result_type: Type,
    pub body: Vec<BodyStatement>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Struct(StructTypes),
    Array(Box<Self>, u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructTypes {
    pub name: String,
    pub types: Vec<StructType>,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructType {
    pub attr_name: String,
    pub attr_type: Type,
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

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue {
    ValueName(ValueName),
    PrimitiveValue(PrimitiveValue),
    StructValue(StructValues),
    FunctionCall(FunctionCall),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expression_value: ExpressionValue,
    pub operation: Option<(ExpressionOperations, Box<Expression>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub name: ValueName,
    pub mutable: bool,
    pub value_type: Option<Type>,
    pub value: Box<Expression>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValues {
    pub name: String,
    pub types: Vec<StructValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValue {
    pub attr_name: String,
    pub attr_value: ValueName,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: FunctionName,
    pub parameters: Vec<Expression>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Great,
    Less,
    Eq,
    GreatEq,
    LessEq,
    NotEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicCondition {
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionCondition {
    pub left: Expression,
    pub condition: Condition,
    pub right: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding {
    pub name: ValueName,
    pub value: Box<Expression>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionLogicCondition {
    pub left: ExpressionCondition,
    pub right: Option<(LogicCondition, Box<ExpressionLogicCondition>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfCondition {
    Single(Expression),
    Logic(ExpressionLogicCondition),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement {
    pub condition: IfCondition,
    pub body: IfBodyStatements,
    pub else_statement: Option<IfBodyStatements>,
    pub else_if_statement: Option<Box<IfStatement>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatements {
    If(Vec<IfBodyStatement>),
    Loop(Vec<IfLoopBodyStatement>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfLoopBodyStatement {
    LetBinding(LetBinding),
    Binding(Binding),
    FunctionCall(FunctionCall),
    If(IfStatement),
    Loop(Vec<LoopBodyStatement>),
    Return(Expression),
    Break,
    Continue,
}

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
