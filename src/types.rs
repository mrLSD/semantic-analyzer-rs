#![allow(dead_code, clippy::module_name_repetitions)]

use crate::ast;
use crate::ast::GetName;
use std::collections::HashMap;

trait TypeAttributes {
    fn get_attribute_index(&self, _attr_name: String) -> Option<u32> {
        None
    }
    fn get_attribute_type(&self, _attr_name: String) -> Option<Type> {
        None
    }
    fn get_method(&self, _method_name: String) -> Option<FunctionName> {
        None
    }
    fn is_attribute(&self, _name: String) -> bool {
        false
    }
    fn is_method(&self, _name: String) -> bool {
        false
    }
}

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

impl ToString for ValueName {
    fn to_string(&self) -> String {
        self.0.clone()
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

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct TypeName(String);

impl GetName for TypeName {
    fn name(&self) -> String {
        self.0.clone()
    }
}

impl From<String> for TypeName {
    fn from(value: String) -> Self {
        Self(value)
    }
}

impl ToString for TypeName {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Struct(StructTypes),
    Array(Box<Self>, u32),
}

impl Type {
    pub fn name(&self) -> TypeName {
        self.to_string().into()
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.name(),
            Self::Struct(struct_type) => struct_type.name.clone(),
            Self::Array(array_type, size) => {
                format!("[{:?};{:?}]", array_type.to_string(), size)
            }
        }
    }
}

impl TypeAttributes for Type {
    fn get_attribute_index(&self, attr_name: String) -> Option<u32> {
        match self {
            Self::Struct(st) => st.get_attribute_index(attr_name),
            _ => None,
        }
    }
    fn get_attribute_type(&self, attr_name: String) -> Option<Type> {
        match self {
            Self::Struct(st) => st.get_attribute_type(attr_name),
            _ => None,
        }
    }
    fn get_method(&self, method_name: String) -> Option<FunctionName> {
        match self {
            Self::Struct(st) => st.get_method(method_name),
            _ => None,
        }
    }
    fn is_attribute(&self, attr_name: String) -> bool {
        match self {
            Self::Struct(st) => st.is_attribute(attr_name),
            _ => false,
        }
    }
    fn is_method(&self, method_name: String) -> bool {
        match self {
            Self::Struct(st) => st.is_method(method_name),
            _ => false,
        }
    }
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

impl GetName for PrimitiveTypes {
    fn name(&self) -> String {
        match self {
            Self::U8 => "u8".to_string(),
            Self::U16 => "u16".to_string(),
            Self::U32 => "u32".to_string(),
            Self::U64 => "u64".to_string(),
            Self::I8 => "i8".to_string(),
            Self::I16 => "i16".to_string(),
            Self::I32 => "i32".to_string(),
            Self::I64 => "i64".to_string(),
            Self::F32 => "f32".to_string(),
            Self::F64 => "f64".to_string(),
            Self::Bool => "bool".to_string(),
            Self::Char => "char".to_string(),
            Self::String => "str".to_string(),
            Self::Ptr => "ptr".to_string(),
            Self::None => "()".to_string(),
        }
    }
}

impl From<ast::PrimitiveTypes> for PrimitiveTypes {
    fn from(value: ast::PrimitiveTypes) -> Self {
        match value {
            ast::PrimitiveTypes::U8 => Self::U8,
            ast::PrimitiveTypes::U16 => Self::U16,
            ast::PrimitiveTypes::U32 => Self::U32,
            ast::PrimitiveTypes::U64 => Self::U64,
            ast::PrimitiveTypes::I8 => Self::I8,
            ast::PrimitiveTypes::I16 => Self::I16,
            ast::PrimitiveTypes::I32 => Self::I32,
            ast::PrimitiveTypes::I64 => Self::I64,
            ast::PrimitiveTypes::F32 => Self::F32,
            ast::PrimitiveTypes::F64 => Self::F64,
            ast::PrimitiveTypes::Bool => Self::Bool,
            ast::PrimitiveTypes::Char => Self::Char,
            ast::PrimitiveTypes::String => Self::String,
            ast::PrimitiveTypes::Ptr => Self::Ptr,
            ast::PrimitiveTypes::None => Self::None,
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructTypes {
    pub name: String,
    pub attributes: HashMap<String, StructType>,
    pub methods: HashMap<String, FunctionName>,
}

impl TypeAttributes for StructTypes {
    fn get_attribute_index(&self, attr_name: String) -> Option<u32> {
        self.attributes.get(&attr_name).map(|attr| attr.attr_index)
    }
    fn get_attribute_type(&self, attr_name: String) -> Option<Type> {
        self.attributes
            .get(&attr_name)
            .map(|attr| attr.attr_type.clone())
    }
    fn get_method(&self, method_name: String) -> Option<FunctionName> {
        self.methods.get(&method_name).cloned()
    }
    fn is_attribute(&self, attr_name: String) -> bool {
        self.attributes.contains_key(&attr_name)
    }
    fn is_method(&self, method_name: String) -> bool {
        self.methods.contains_key(&method_name)
    }
}

impl From<ast::StructTypes<'_>> for StructTypes {
    fn from(value: ast::StructTypes<'_>) -> Self {
        Self {
            name: value.name(),
            attributes: {
                let mut res = HashMap::new();
                for (index, val) in value.attributes.iter().enumerate() {
                    let name = (*val.attr_name.fragment()).to_string();
                    let mut v: StructType = val.clone().into();
                    v.attr_index = index as u32;
                    res.insert(name, v);
                }
                res
            },
            methods: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructType {
    pub attr_name: String,
    pub attr_index: u32,
    pub attr_type: Type,
}

impl From<ast::StructType<'_>> for StructType {
    fn from(value: ast::StructType<'_>) -> Self {
        Self {
            attr_name: value.name(),
            attr_type: value.attr_type.into(),
            attr_index: 0,
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
pub struct StructValues {
    pub name: String,
    pub types: Vec<StructValue>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Great,
    Less,
    Eq,
    GreatEq,
    LessEq,
    NotEq,
}

impl From<ast::Condition> for Condition {
    fn from(value: ast::Condition) -> Self {
        match value {
            ast::Condition::Great => Self::Great,
            ast::Condition::Less => Self::Less,
            ast::Condition::Eq => Self::Eq,
            ast::Condition::GreatEq => Self::GreatEq,
            ast::Condition::LessEq => Self::LessEq,
            ast::Condition::NotEq => Self::NotEq,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicCondition {
    And,
    Or,
}

impl From<ast::LogicCondition> for LogicCondition {
    fn from(value: ast::LogicCondition) -> Self {
        match value {
            ast::LogicCondition::And => Self::And,
            ast::LogicCondition::Or => Self::Or,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionCondition {
    pub left: Expression,
    pub condition: Condition,
    pub right: Expression,
}

impl From<ast::ExpressionCondition<'_>> for ExpressionCondition {
    fn from(value: ast::ExpressionCondition<'_>) -> Self {
        Self {
            left: value.left.into(),
            condition: value.condition.into(),
            right: value.right.into(),
        }
    }
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

impl From<ast::ExpressionLogicCondition<'_>> for ExpressionLogicCondition {
    fn from(value: ast::ExpressionLogicCondition<'_>) -> Self {
        Self {
            left: value.left.into(),
            right: value
                .right
                .map(|(v, expr)| (v.into(), Box::new(expr.as_ref().clone().into()))),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfCondition {
    Single(Expression),
    Logic(ExpressionLogicCondition),
}

impl From<ast::IfCondition<'_>> for IfCondition {
    fn from(value: ast::IfCondition<'_>) -> Self {
        match value {
            ast::IfCondition::Single(v) => Self::Single(v.into()),
            ast::IfCondition::Logic(v) => Self::Logic(v.into()),
        }
    }
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
            else_statement: value.else_statement.map(Into::into),
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

impl From<ast::IfBodyStatements<'_>> for IfBodyStatements {
    fn from(value: ast::IfBodyStatements<'_>) -> Self {
        match value {
            ast::IfBodyStatements::If(v) => Self::If(v.iter().map(|v| v.clone().into()).collect()),
            ast::IfBodyStatements::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
        }
    }
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
            ast::LoopBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::LoopBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::LoopBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::LoopBodyStatement::If(v) => Self::If(v.into()),
            ast::LoopBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::LoopBodyStatement::Return(v) => Self::Return(v.into()),
            ast::LoopBodyStatement::Break => Self::Break,
            ast::LoopBodyStatement::Continue => Self::Continue,
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

impl From<ast::IfBodyStatement<'_>> for IfBodyStatement {
    fn from(value: ast::IfBodyStatement<'_>) -> Self {
        match value {
            ast::IfBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::IfBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::IfBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::IfBodyStatement::If(v) => Self::If(v.into()),
            ast::IfBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::IfBodyStatement::Return(v) => Self::Return(v.into()),
        }
    }
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

impl From<ast::IfLoopBodyStatement<'_>> for IfLoopBodyStatement {
    fn from(value: ast::IfLoopBodyStatement<'_>) -> Self {
        match value {
            ast::IfLoopBodyStatement::LetBinding(v) => Self::LetBinding(v.into()),
            ast::IfLoopBodyStatement::Binding(v) => Self::Binding(v.into()),
            ast::IfLoopBodyStatement::FunctionCall(v) => Self::FunctionCall(v.into()),
            ast::IfLoopBodyStatement::If(v) => Self::If(v.into()),
            ast::IfLoopBodyStatement::Loop(v) => {
                Self::Loop(v.iter().map(|v| v.clone().into()).collect())
            }
            ast::IfLoopBodyStatement::Return(v) => Self::Return(v.into()),
            ast::IfLoopBodyStatement::Break => Self::Break,
            ast::IfLoopBodyStatement::Continue => Self::Continue,
        }
    }
}

#[allow(clippy::module_name_repetitions)]
pub mod error {
    use crate::ast::CodeLocation;

    #[derive(Debug, Clone)]
    pub struct EmptyError;

    #[derive(Debug, Clone)]
    pub struct StateError(pub StateErrorKind);

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
        ValueIsNotMutable,
        FunctionNotFound,
        FunctionParameterTypeWrong,
        ReturnNotFound,
        IfElseDuplicated,
        TypeNotFound,
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
}
