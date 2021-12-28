#![allow(dead_code)]
use nom_locate::LocatedSpan;

pub type Ident<'a> = LocatedSpan<&'a str>;

pub trait GetName {
    fn name(&self) -> String;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImportName<'a>(Ident<'a>);

impl GetName for ImportName<'_> {
    fn name(&self) -> String {
        self.0.fragment().to_string()
    }
}

pub type ImportPath<'a> = Vec<ImportName<'a>>;

#[derive(Debug, Clone, PartialEq)]
pub struct ConstantName<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionName<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterName<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct ValueName<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq)]
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType<'a> {
    pub attr_name: Ident<'a>,
    pub attr_type: Type<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructTypes<'a> {
    pub name: Ident<'a>,
    pub types: Vec<StructType<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type<'a> {
    Primitive(PrimitiveTypes),
    Struct(StructTypes<'a>),
    Array(Box<Self>),
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

impl GetName for Constant<'_> {
    fn name(&self) -> String {
        self.name.0.fragment().to_string()
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
    pub result_type: Vec<Type<'a>>,
    pub body: Vec<BodyStatement<'a>>,
}

impl GetName for FunctionStatement<'_> {
    fn name(&self) -> String {
        self.name.0.fragment().to_string()
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
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue<'a> {
    ValueName(ValueName<'a>),
    PrimitiveValue(PrimitiveValue),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Debug, Clone, PartialEq)]
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
pub struct Expression<'a> {
    pub expression_value: ExpressionValue<'a>,
    pub operation: Option<(ExpressionOperations, ExpressionValue<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding<'a> {
    pub name: ValueName<'a>,
    pub value_type: Option<Type<'a>>,
    pub value: Box<BodyStatement<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall<'a> {
    pub name: FunctionName<'a>,
    pub parameters: Vec<Expression<'a>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement<'a> {
    pub condition: Expression<'a>,
    pub body: Box<BodyStatement<'a>>,
    pub else_statement: Option<Box<BodyStatement<'a>>>,
    pub else_if_statement: Option<Box<IfStatement<'a>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Box<BodyStatement<'a>>),
    Expression(Expression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MainStatement<'a> {
    Import(ImportPath<'a>),
    Constant(Constant<'a>),
    Function(FunctionStatement<'a>),
}

pub type Main<'a> = Vec<MainStatement<'a>>;
