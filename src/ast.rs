#![allow(dead_code)]
use nom_locate::LocatedSpan;

pub type Ident<'a> = LocatedSpan<&'a [u8]>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ImportName<'a>(Ident<'a>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ConstantName<'a>(Ident<'a>);

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionName<'a>(Ident<'a>);

#[derive(Debug, Clone, Eq, PartialEq)]
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

pub struct StructType<'a> {
    pub attr_name: Ident<'a>,
    pub attr_type: Type<'a>,
}

pub struct StructTypes<'a> {
    pub name: Ident<'a>,
    pub types: Vec<StructType<'a>>,
}

pub enum Type<'a> {
    Primitive(PrimitiveTypes),
    Struct(StructTypes<'a>),
    Array(Box<Self>),
}

pub struct ConstantValue<'a>(Ident<'a>);

pub struct Constant<'a> {
    name: ConstantName<'a>,
    constant_type: Type<'a>,
    constant_value: ConstantValue<'a>,
}

pub enum MainStatement<'a> {
    Import(Vec<ImportName<'a>>),
    Constant(Constant<'a>),
    Function(FunctionName<'a>),
}

pub type Main<'a> = Vec<MainStatement<'a>>;
