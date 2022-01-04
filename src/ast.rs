#![allow(dead_code)]
use nom_locate::LocatedSpan;

pub type Ident<'a> = LocatedSpan<&'a [u8]>;

#[derive(Debug, Clone, PartialEq)]
pub struct ImportName<'a>(Ident<'a>);

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
pub struct ConstantValue<'a>(Ident<'a>);

#[derive(Debug, Clone, PartialEq)]
pub struct Constant<'a> {
    name: ConstantName<'a>,
    constant_type: Type<'a>,
    constant_value: ConstantValue<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter<'a> {
    pub name: ParameterName<'a>,
    pub parameter_type: Type<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement<'a> {
    pub name: FunctionName<'a>,
    pub parameter: Vec<Parameter<'a>>,
    pub result_type: Type<'a>,
    pub body: BodyStatement<'a>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    pub expression_value: ExpressionValue<'a>,
    pub operation: Option<(ExpressionOperations, ExpressionValue<'a>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ValueState<'a> {
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
pub enum BodyStatement<'a> {
    LetBinding(ValueState<'a>),
    FunctionCall(FunctionCall<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum MainStatement<'a> {
    Import(Vec<ImportName<'a>>),
    Constant(Constant<'a>),
    Function(FunctionStatement<'a>),
}

pub type Main<'a> = Vec<MainStatement<'a>>;
