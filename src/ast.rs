//! # AST
//! Abstract Syntax Tree representation contains tree node that
//! represent full cycle of the program, and represent
//! `turing-complete` state machine.
#![allow(dead_code)]

use nom_locate::LocatedSpan;

/// Max priority level fpr expressions operations
pub const MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS: u8 = 9;

/// Basic `Ident` entity
pub type Ident<'a> = LocatedSpan<&'a str>;

pub trait GetName {
    fn name(&self) -> String;
}

pub trait GetType {
    fn inner_type(&self) -> String;
}

pub trait GetLocation {
    fn location(&self) -> CodeLocation;
}

/// Specific import name
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportName<'a>(Ident<'a>);

impl GetName for ImportName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

/// Imports with full path of import
pub type ImportPath<'a> = Vec<ImportName<'a>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantName<'a>(Ident<'a>);

impl<'a> ConstantName<'a> {
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for ConstantName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl GetName for ConstantName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionName<'a>(Ident<'a>);

impl<'a> FunctionName<'a> {
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for FunctionName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl<'a> ToString for FunctionName<'a> {
    fn to_string(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParameterName<'a>(Ident<'a>);

impl<'a> ParameterName<'a> {
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl ToString for ParameterName<'_> {
    fn to_string(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueName<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeLocation(u32, usize);

impl CodeLocation {
    pub const fn new(location: u32, offset: usize) -> Self {
        Self(location, offset)
    }

    pub const fn line(&self) -> u32 {
        self.0
    }

    pub const fn offset(&self) -> usize {
        self.1
    }
}

impl<'a> ValueName<'a> {
    pub const fn new(name: Ident<'a>) -> Self {
        Self(name)
    }
}

impl GetLocation for ValueName<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.0.location_line(), self.0.location_offset())
    }
}

impl GetName for ValueName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructValue<'a> {
    pub name: ValueName<'a>,
    pub attribute: ValueName<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType<'a> {
    pub attr_name: Ident<'a>,
    pub attr_type: Type<'a>,
}

impl GetName for StructType<'_> {
    fn name(&self) -> String {
        (*self.attr_name.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypes<'a> {
    pub name: Ident<'a>,
    pub attributes: Vec<StructType<'a>>,
}

impl GetLocation for StructTypes<'_> {
    fn location(&self) -> CodeLocation {
        CodeLocation::new(self.name.location_line(), self.name.location_offset())
    }
}

impl<'a> GetName for StructTypes<'a> {
    fn name(&self) -> String {
        (*self.name.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Primitive(PrimitiveTypes),
    Struct(StructTypes<'a>),
    Array(Box<Self>, u32),
}

impl<'a> GetName for Type<'a> {
    fn name(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.name(),
            Self::Struct(struct_type) => (*struct_type.name.fragment()).to_string(),
            Self::Array(array_type, size) => {
                format!("[{:?};{:?}]", array_type.name(), size)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue<'a> {
    Constant(ConstantName<'a>),
    Value(PrimitiveValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression<'a> {
    pub value: ConstantValue<'a>,
    pub operation: Option<(ExpressionOperations, Box<ConstantExpression<'a>>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant<'a> {
    pub name: ConstantName<'a>,
    pub constant_type: Type<'a>,
    pub constant_value: ConstantExpression<'a>,
}

impl GetLocation for Constant<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for Constant<'_> {
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter<'a> {
    pub name: ParameterName<'a>,
    pub parameter_type: Type<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement<'a> {
    pub name: FunctionName<'a>,
    pub parameters: Vec<FunctionParameter<'a>>,
    pub result_type: Type<'a>,
    pub body: Vec<BodyStatement<'a>>,
}

impl GetLocation for FunctionStatement<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for FunctionStatement<'_> {
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
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

impl PrimitiveValue {
    pub const fn get_type(&self) -> Type<'_> {
        match self {
            Self::U8(_) => Type::Primitive(PrimitiveTypes::U8),
            Self::U16(_) => Type::Primitive(PrimitiveTypes::U16),
            Self::U32(_) => Type::Primitive(PrimitiveTypes::U32),
            Self::U64(_) => Type::Primitive(PrimitiveTypes::U64),
            Self::I8(_) => Type::Primitive(PrimitiveTypes::I8),
            Self::I16(_) => Type::Primitive(PrimitiveTypes::I16),
            Self::I32(_) => Type::Primitive(PrimitiveTypes::I32),
            Self::I64(_) => Type::Primitive(PrimitiveTypes::I64),
            Self::F32(_) => Type::Primitive(PrimitiveTypes::F32),
            Self::F64(_) => Type::Primitive(PrimitiveTypes::F64),
            Self::Char(_) => Type::Primitive(PrimitiveTypes::Char),
            Self::Bool(_) => Type::Primitive(PrimitiveTypes::Bool),
            Self::String(_) => Type::Primitive(PrimitiveTypes::String),
            Self::Ptr => Type::Primitive(PrimitiveTypes::Ptr),
            Self::None => Type::Primitive(PrimitiveTypes::None),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue<'a> {
    ValueName(ValueName<'a>),
    PrimitiveValue(PrimitiveValue),
    FunctionCall(FunctionCall<'a>),
    StructValue(StructValue<'a>),
    Expression(Box<Expression<'a>>),
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

impl ExpressionOperations {
    pub const fn priority(&self) -> u8 {
        match self {
            Self::Plus => 5,
            Self::Minus => 4,
            Self::Divide => 8,
            Self::Multiply | Self::ShiftLeft | Self::ShiftRight => {
                MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
            }
            Self::Or | Self::Xor => 6,
            Self::And
            | Self::Eq
            | Self::NotEq
            | Self::Great
            | Self::Less
            | Self::GreatEq
            | Self::LessEq => 7,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    pub expression_value: ExpressionValue<'a>,
    pub operation: Option<(ExpressionOperations, Box<Expression<'a>>)>,
}

impl GetLocation for Expression<'_> {
    fn location(&self) -> CodeLocation {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding<'a> {
    pub name: ValueName<'a>,
    pub mutable: bool,
    pub value_type: Option<Type<'a>>,
    pub value: Box<Expression<'a>>,
}

impl GetLocation for LetBinding<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for LetBinding<'_> {
    fn name(&self) -> String {
        self.name.0.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binding<'a> {
    pub name: ValueName<'a>,
    pub value: Box<Expression<'a>>,
}

impl GetLocation for Binding<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for Binding<'_> {
    fn name(&self) -> String {
        self.name.0.to_string()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall<'a> {
    pub name: FunctionName<'a>,
    pub parameters: Vec<Expression<'a>>,
}

impl GetLocation for FunctionCall<'_> {
    fn location(&self) -> CodeLocation {
        self.name.location()
    }
}

impl GetName for FunctionCall<'_> {
    fn name(&self) -> String {
        (*self.name.0.fragment()).to_string()
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
pub struct ExpressionCondition<'a> {
    pub left: Expression<'a>,
    pub condition: Condition,
    pub right: Expression<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionLogicCondition<'a> {
    pub left: ExpressionCondition<'a>,
    pub right: Option<(LogicCondition, Box<ExpressionLogicCondition<'a>>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfCondition<'a> {
    Single(Expression<'a>),
    Logic(ExpressionLogicCondition<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement<'a> {
    pub condition: IfCondition<'a>,
    pub body: IfBodyStatements<'a>,
    pub else_statement: Option<IfBodyStatements<'a>>,
    pub else_if_statement: Option<Box<IfStatement<'a>>>,
}

impl GetLocation for IfStatement<'_> {
    fn location(&self) -> CodeLocation {
        todo!()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    Binding(Binding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Vec<LoopBodyStatement<'a>>),
    Expression(Expression<'a>),
    Return(Expression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    Binding(Binding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Vec<LoopBodyStatement<'a>>),
    Return(Expression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfLoopBodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    Binding(Binding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Vec<LoopBodyStatement<'a>>),
    Return(Expression<'a>),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatements<'a> {
    If(Vec<IfBodyStatement<'a>>),
    Loop(Vec<IfLoopBodyStatement<'a>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LoopBodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    Binding(Binding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Vec<LoopBodyStatement<'a>>),
    Return(Expression<'a>),
    Break,
    Continue,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MainStatement<'a> {
    Import(ImportPath<'a>),
    Constant(Constant<'a>),
    Types(StructTypes<'a>),
    Function(FunctionStatement<'a>),
}

pub type Main<'a> = Vec<MainStatement<'a>>;
