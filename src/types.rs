#![allow(dead_code, clippy::module_name_repetitions)]

use crate::ast;
use crate::ast::GetName;

/// State result type - for single results
pub type StateResult<T> = Result<T, error::StateError>;

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Struct(StructTypes),
    Array(Box<Self>, u32),
}

impl From<ast::Type<'_>> for Type {
    fn from(value: ast::Type<'_>) -> Self {
        match value {
            ast::Type::Primitive(v) => Self::Primitive(v.into()),
            ast::Type::Struct(v) => Self::Struct(v.into()),
            ast::Type::Array(v, s) => Self::Array(Box::new(v.as_ref().clone().into()), s),
        }
    }
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

impl From<ast::PrimitiveTypes> for PrimitiveTypes {
    fn from(value: ast::PrimitiveTypes) -> Self {
        match value {
            ast::PrimitiveTypes::U8 => PrimitiveTypes::U8,
            ast::PrimitiveTypes::U16 => PrimitiveTypes::U16,
            ast::PrimitiveTypes::U32 => PrimitiveTypes::U32,
            ast::PrimitiveTypes::U64 => PrimitiveTypes::U64,
            ast::PrimitiveTypes::I8 => PrimitiveTypes::I8,
            ast::PrimitiveTypes::I16 => PrimitiveTypes::I16,
            ast::PrimitiveTypes::I32 => PrimitiveTypes::I32,
            ast::PrimitiveTypes::I64 => PrimitiveTypes::I64,
            ast::PrimitiveTypes::F32 => PrimitiveTypes::F32,
            ast::PrimitiveTypes::F64 => PrimitiveTypes::F64,
            ast::PrimitiveTypes::Bool => PrimitiveTypes::Bool,
            ast::PrimitiveTypes::Char => PrimitiveTypes::Char,
            ast::PrimitiveTypes::String => PrimitiveTypes::String,
            ast::PrimitiveTypes::Ptr => PrimitiveTypes::Ptr,
            ast::PrimitiveTypes::None => PrimitiveTypes::None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructTypes {
    pub name: String,
    pub types: Vec<StructType>,
}

impl From<ast::StructTypes<'_>> for StructTypes {
    fn from(value: ast::StructTypes<'_>) -> Self {
        Self {
            name: value.name(),
            types: value.types.iter().map(|v| v.clone().into()).collect(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructType {
    pub attr_name: String,
    pub attr_type: Type,
}

impl From<ast::StructType<'_>> for StructType {
    fn from(value: ast::StructType<'_>) -> Self {
        Self {
            attr_name: value.name(),
            attr_type: value.attr_type.into(),
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
            ast::BodyStatement::LetBinding(v) => BodyStatement::LetBinding(v.into()),
            ast::BodyStatement::Binding(v) => BodyStatement::Binding(v.into()),
            ast::BodyStatement::FunctionCall(v) => BodyStatement::FunctionCall(v.into()),
            ast::BodyStatement::If(v) => BodyStatement::If(v.into()),
            ast::BodyStatement::Loop(v) => {
                BodyStatement::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::BodyStatement::Expression(v) => BodyStatement::Expression(v.into()),
            ast::BodyStatement::Return(v) => BodyStatement::Return(v.into()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue {
    ValueName(ValueName),
    PrimitiveValue(PrimitiveValue),
    StructValue(StructValues),
    FunctionCall(FunctionCall),
}

impl From<ast::ExpressionValue<'_>> for ExpressionValue {
    fn from(value: ast::ExpressionValue<'_>) -> Self {
        match value {
            ast::ExpressionValue::ValueName(v) => ExpressionValue::ValueName(v.into()),
            ast::ExpressionValue::PrimitiveValue(v) => ExpressionValue::PrimitiveValue(v.into()),
            ast::ExpressionValue::StructValue(v) => ExpressionValue::StructValue(v.into()),
            ast::ExpressionValue::FunctionCall(v) => ExpressionValue::FunctionCall(v.into()),
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

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub expression_value: ExpressionValue,
    pub operation: Option<(ExpressionOperations, Box<Expression>)>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding {
    pub name: ValueName,
    pub mutable: bool,
    pub value_type: Option<Type>,
    pub value: Box<Expression>,
}

impl From<ast::LetBinding<'_>> for LetBinding {
    fn from(value: ast::LetBinding<'_>) -> Self {
        Self {
            name: value.name.into(),
            mutable: value.mutable,
            value_type: value.value_type.map(|v| v.into()),
            value: Box::new(value.value_type.as_ref().clone().into()),
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
            ast::PrimitiveValue::U8(v) => PrimitiveValue::U8(v),
            ast::PrimitiveValue::U16(v) => PrimitiveValue::U16(v),
            ast::PrimitiveValue::U32(v) => PrimitiveValue::U32(v),
            ast::PrimitiveValue::U64(v) => PrimitiveValue::U64(v),
            ast::PrimitiveValue::I8(v) => PrimitiveValue::I8(v),
            ast::PrimitiveValue::I16(v) => PrimitiveValue::I16(v),
            ast::PrimitiveValue::I32(v) => PrimitiveValue::I32(v),
            ast::PrimitiveValue::I64(v) => PrimitiveValue::I64(v),
            ast::PrimitiveValue::F32(v) => PrimitiveValue::F32(v),
            ast::PrimitiveValue::F64(v) => PrimitiveValue::F64(v),
            ast::PrimitiveValue::Bool(v) => PrimitiveValue::Bool(v),
            ast::PrimitiveValue::String(v) => PrimitiveValue::String(v),
            ast::PrimitiveValue::Char(v) => PrimitiveValue::Char(v),
            ast::PrimitiveValue::Ptr => PrimitiveValue::Ptr,
            ast::PrimitiveValue::None => PrimitiveValue::None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValues {
    pub name: String,
    pub types: Vec<StructValue>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValue {
    pub name: ValueName,
    pub attribute: ValueName,
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

impl From<ast::FunctionCall<'_>> for FunctionCall {
    fn from(value: ast::FunctionCall<'_>) -> Self {
        Self {
            name: value.name.into(),
            parameters: value.iter().map(|v| v.into()).collect(),
        }
    }
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

impl From<ast::Binding<'_>> for Binding {
    fn from(value: ast::Binding<'_>) -> Self {
        Self {
            name: value.name.into(),
            value: Box::new(value.value.as_ref().clone().into()),
        }
    }
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

impl From<ast::IfStatement<'_>> for IfStatement {
    fn from(value: ast::IfStatement<'_>) -> Self {
        Self {
            condition: value.condition.into(),
            body: value.body.into(),
            else_statement: value.else_statement.map(|v| v.into()),
            else_if_statement: value
                .else_if_statement
                .map(|v| Box::new(v.as_ref().clone().into())),
        }
    }
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

impl From<ast::LoopBodyStatement<'_>> for LoopBodyStatement {
    fn from(value: ast::LoopBodyStatement<'_>) -> Self {
        match value {
            ast::LoopBodyStatement::LetBinding(v) => LoopBodyStatement::LetBinding(v.into()),
            ast::LoopBodyStatement::Binding(v) => LoopBodyStatement::Binding(v.into()),
            ast::LoopBodyStatement::FunctionCall(v) => LoopBodyStatement::FunctionCall(v.into()),
            ast::LoopBodyStatement::If(v) => LoopBodyStatement::If(v.into()),
            ast::LoopBodyStatement::Loop(v) => {
                LoopBodyStatement::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::LoopBodyStatement::Return(v) => LoopBodyStatement::Return(v.into()),
            ast::LoopBodyStatement::Break => LoopBodyStatement::Break,
            ast::LoopBodyStatement::Continue => LoopBodyStatement::Continue,
        }
    }
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
