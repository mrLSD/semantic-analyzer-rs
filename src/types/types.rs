//! # Semantic types
//! Type-system types for Semantic analyzer State results.

use super::{FunctionName, ValueName};
use crate::ast::{self, GetName};
use std::collections::HashMap;

/// `TypeAttributes` type attributes trait.
/// Used for types declarations.
pub trait TypeAttributes {
    /// Get attribute index by value name for the parent type
    fn get_attribute_index(&self, attr_name: &ValueName) -> Option<u32>;
    /// Get attribute type by value name for the parent type
    fn get_attribute_type(&self, attr_name: &ValueName) -> Option<Type>;
    /// Get function name for the parent type by method name
    fn get_method(&self, method_name: String) -> Option<FunctionName>;
    /// Check is value attribute
    fn is_attribute(&self, name: &ValueName) -> bool;
    /// Check is name is method
    fn is_method(&self, name: String) -> bool;
}

/// Type name representation
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

/// # Type
/// Basic representation of Type. Basic entities:
/// - primitive type
/// - struct type
/// - array type
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Primitive(PrimitiveTypes),
    Struct(StructTypes),
    Array(Box<Self>, u32),
}

impl Type {
    /// Get type name
    pub fn name(&self) -> TypeName {
        self.to_string().into()
    }

    /// Get structure type if it is
    pub fn get_struct(&self) -> Option<StructTypes> {
        match self {
            Self::Struct(ty) => Some(ty.clone()),
            _ => None,
        }
    }
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Self::Primitive(primitive) => primitive.to_string(),
            Self::Struct(struct_type) => struct_type.name.clone(),
            Self::Array(array_type, size) => {
                format!("[{:?};{:?}]", array_type.to_string(), size)
            }
        }
    }
}

impl TypeAttributes for Type {
    fn get_attribute_index(&self, attr_name: &ValueName) -> Option<u32> {
        match self {
            Self::Struct(st) => st.get_attribute_index(attr_name),
            _ => None,
        }
    }
    fn get_attribute_type(&self, attr_name: &ValueName) -> Option<Type> {
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
    fn is_attribute(&self, attr_name: &ValueName) -> bool {
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

/// # Primitive types
/// Most primitive type. It's basic elements for other types.
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

impl ToString for PrimitiveTypes {
    fn to_string(&self) -> String {
        let s = match self {
            Self::U8 => "u8",
            Self::U16 => "u16",
            Self::U32 => "u32",
            Self::U64 => "u64",
            Self::I8 => "i8",
            Self::I16 => "i16",
            Self::I32 => "i32",
            Self::I64 => "i64",
            Self::F32 => "f32",
            Self::F64 => "f64",
            Self::Bool => "bool",
            Self::Char => "char",
            Self::String => "str",
            Self::Ptr => "ptr",
            Self::None => "()",
        };
        s.to_string()
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

/// # Struct types
/// Basic entity for struct type itself.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructTypes {
    /// Type name
    pub name: String,
    /// Struct attributes
    pub attributes: HashMap<ValueName, StructAttributeType>,
    /// Struct methods
    pub methods: HashMap<String, FunctionName>,
}

impl TypeAttributes for StructTypes {
    fn get_attribute_index(&self, attr_name: &ValueName) -> Option<u32> {
        self.attributes.get(attr_name).map(|attr| attr.attr_index)
    }
    fn get_attribute_type(&self, attr_name: &ValueName) -> Option<Type> {
        self.attributes
            .get(attr_name)
            .map(|attr| attr.attr_type.clone())
    }
    fn get_method(&self, method_name: String) -> Option<FunctionName> {
        self.methods.get(&method_name).cloned()
    }
    fn is_attribute(&self, attr_name: &ValueName) -> bool {
        self.attributes.contains_key(attr_name)
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
                    let mut v: StructAttributeType = val.clone().into();
                    v.attr_index = index as u32;
                    res.insert(name.into(), v);
                }
                res
            },
            methods: HashMap::new(),
        }
    }
}

/// `StructAttributeType` is type for Struct attributes fields.
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct StructAttributeType {
    /// Attribute name for struct type
    pub attr_name: ValueName,
    /// Attribute index representation for for struct type
    pub attr_index: u32,
    /// Attribute type for struct type
    pub attr_type: Type,
}

impl From<ast::StructType<'_>> for StructAttributeType {
    fn from(value: ast::StructType<'_>) -> Self {
        Self {
            attr_name: value.name().into(),
            attr_type: value.attr_type.into(),
            attr_index: 0,
        }
    }
}
