//! # AST
//! Abstract Syntax Tree representation contains tree node that
//! represent full cycle and aspects of the programming language, and
//! represent `Turing-complete` state machine.

use nom_locate::LocatedSpan;

/// Max priority level fpr expressions operations
pub const MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS: u8 = 9;

/// Basic `Ident` entity for elements of AST
pub type Ident<'a> = LocatedSpan<&'a str>;

/// `GetName` trait, represent name of specific entity.
pub trait GetName {
    fn name(&self) -> String;
}

/// `GetLocation` represent location of source data for AST element.
/// Useful to locate specific source code location, espessialt for `Ident`.
pub trait GetLocation {
    fn location(&self) -> CodeLocation;
}

/// Import name element of AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportName<'a>(Ident<'a>);

impl GetName for ImportName<'_> {
    fn name(&self) -> String {
        (*self.0.fragment()).to_string()
    }
}

impl<'a> ImportName<'a> {
    pub const fn new(ident: Ident<'a>) -> Self {
        Self(ident)
    }
}

/// Imports with full path of import
pub type ImportPath<'a> = Vec<ImportName<'a>>;

/// `ConstantName` constant name for `Constant` elements of AST
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstantName<'a>(Ident<'a>);

impl<'a> ConstantName<'a> {
    /// Init `ConstantName`, especially useful for testing
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

/// `FunctionName` function name for `Function` elements of AST.
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

/// `ParameterName` parameter name element of AST, used for `Function`
/// parameters declaration.
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

/// `ValueName` value name element of AST. It's basic entity for:
/// - `Struct` type declaration
/// - `LetBinding` declaration
/// - `Binding` declaration
/// - `ExpressionValue` declaration
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ValueName<'a>(Ident<'a>);

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

/// `CodeLocation` code location of source for AST elements.
/// Contains: `line` nad `position`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeLocation(u32, usize);

impl CodeLocation {
    /// Initialize code location with:
    /// - `location` - line of source code
    /// - `offset` - position on the line of source code
    pub const fn new(location: u32, offset: usize) -> Self {
        Self(location, offset)
    }

    /// Get location line in the source
    pub const fn line(&self) -> u32 {
        self.0
    }

    /// Get location position on the line in the source
    pub const fn offset(&self) -> usize {
        self.1
    }
}

/// `PrimitiveTypes` primitive types elements of AST.
/// It's represent basic (primitive) types.
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

/// `ExpressionStructValue` expression struct value element of AST.
/// Used for expression value declaration. The basic entity is:
/// - value name of struct type
/// - value struct type attribute
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExpressionStructValue<'a> {
    /// Value name of struct typed value
    pub name: ValueName<'a>,
    /// Attribute name of struct typed value
    pub attribute: ValueName<'a>,
}

/// `StructType` struct type basic element used for `StructTypes`.
/// It contains basic elements:
/// - attribute name
/// - attribute type
#[derive(Debug, Clone, PartialEq)]
pub struct StructType<'a> {
    /// Attribute name entity of struct type
    pub attr_name: Ident<'a>,
    /// Attribute type entity of struct type
    pub attr_type: Type<'a>,
}

impl GetName for StructType<'_> {
    fn name(&self) -> String {
        (*self.attr_name.fragment()).to_string()
    }
}

/// `StructTypes` struct type element of AST.
/// Basic entity to declare struct complex types and its attributes.
#[derive(Debug, Clone, PartialEq)]
pub struct StructTypes<'a> {
    /// Struct type name
    pub name: Ident<'a>,
    /// Struct type attributes
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

/// `Type` element of AST.
/// Basic entity that represents types. Basic type entity is:
/// - Primitive types
/// - Struct types
/// - Arrays
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

/// `ConstantValue` constant value element of AST.
/// Used for `ConstantExpression`. Constant value basic entities:
/// - `constant` it can contain other constant
/// - `value` - primitive value (like numbers etc.)
#[derive(Debug, Clone, PartialEq)]
pub enum ConstantValue<'a> {
    Constant(ConstantName<'a>),
    Value(PrimitiveValue),
}

/// `ConstantExpression` constant expression element of AST.
/// Used to declare constants.
/// The constant expression value based on other constant or primitive values.
/// Constant Expression can contain optional expression operation with other
/// constant expression. So it can be represented as Constant expression
/// tree as operations with other constant expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct ConstantExpression<'a> {
    /// Constant value - can be other constant 0r primitive value
    pub value: ConstantValue<'a>,
    /// Constant expression optional expression operation with other constant expression declarations.
    pub operation: Option<(ExpressionOperations, Box<ConstantExpression<'a>>)>,
}

/// `Constant` constant declaration element of AST.
/// Basic constant entity contains:
/// - constant name
/// - constant type
/// - constant value - based on constant expression
#[derive(Debug, Clone, PartialEq)]
pub struct Constant<'a> {
    /// Constant name
    pub name: ConstantName<'a>,
    /// Constant type
    pub constant_type: Type<'a>,
    /// Constant value based on constant expression
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

/// `FunctionParameter` function parameter element of AST.
/// Used for `FunctionStatement` declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionParameter<'a> {
    /// Function parameter name
    pub name: ParameterName<'a>,
    /// Function parameter type
    pub parameter_type: Type<'a>,
}

/// `FunctionStatement` it's one of the most basic element of AST.
/// Basic entity of program logic. It contains function declaration and
/// function body.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionStatement<'a> {
    /// Function name
    pub name: FunctionName<'a>,
    /// Function parameters
    pub parameters: Vec<FunctionParameter<'a>>,
    /// Function result type
    pub result_type: Type<'a>,
    /// Function body
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

/// `PrimitiveValue` represents primitive value element of AST.
/// Values based on primitive types.
/// Used for `ConstantValue` and `ExpressionValue`.
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

/// `ExpressionValue` expression value element of AST.
/// Basic entity for `Expression` elements of AST.
/// Expression value contains entities:
/// - `ValueName` - value name of expression
/// - `PrimitiveValue` - primitive value name of expression (for
///   example, it's numbers: 10, 3.2 and other primitive values)
/// - `FunctionCall` - function call (with parameters) of expression
/// - `StructValue` - value of expression based on `Struct` types.
/// - `Expression` - expression representation (sub branch)
#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionValue<'a> {
    /// Value name of expression
    ValueName(ValueName<'a>),
    /// Primitive value of expression (like numbers etc.)
    PrimitiveValue(PrimitiveValue),
    /// Function call (with parameters) of expression
    FunctionCall(FunctionCall<'a>),
    /// Value of expression based on `Struct` types.
    StructValue(ExpressionStructValue<'a>),
    /// Expression representation (sub branch)
    Expression(Box<Expression<'a>>),
}

/// `ExpressionOperations` expression operation element of AST.
/// Used for expression operations:
/// - `ConstantExpression` - expression of constant declaration
/// - `Expression` - part of operations for expressions
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

/// `Expression` element of AST is basic entity that
/// represent expression, and optionally expression with optional
/// operations with other expression. So it can be expression tree
/// with expression operations.
#[derive(Debug, Clone, PartialEq)]
pub struct Expression<'a> {
    /// Expression value itself
    pub expression_value: ExpressionValue<'a>,
    /// Optional expression operation with other expression value
    pub operation: Option<(ExpressionOperations, Box<Expression<'a>>)>,
}

impl GetLocation for Expression<'_> {
    fn location(&self) -> CodeLocation {
        // TODO: extend it
        CodeLocation::new(1, 0)
    }
}

/// `LetBinding` let binding element of AST. Basic entity
/// for `values` declarations.
#[derive(Debug, Clone, PartialEq)]
pub struct LetBinding<'a> {
    /// Value name of let binding
    pub name: ValueName<'a>,
    /// Mutability flag of binding
    pub mutable: bool,
    /// Optional type of value
    pub value_type: Option<Type<'a>>,
    /// Value expression to bind as result of let bending
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

/// `Binding` binding element of AST. Basic entity
/// for `values` re-declaration, to bind new values for already
/// declared values.
#[derive(Debug, Clone, PartialEq)]
pub struct Binding<'a> {
    /// Binding value name
    pub name: ValueName<'a>,
    /// Value expression as result of binding
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

/// `FunctionCall` function call element of AST.
/// Basic entity for function call representation.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall<'a> {
    /// Function name of called function
    pub name: FunctionName<'a>,
    /// Function parameters, represented through expression
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

/// `Condition` condition declarations.
/// Used for `ExpressionCondition`. Contains basic condition
/// entities: `<. >, ==, <=, >=, !=`
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Condition {
    Great,
    Less,
    Eq,
    GreatEq,
    LessEq,
    NotEq,
}

/// `LogicCondition` declaration of logical condition operation.
/// It can contains only: `and`, `or`. Used for `IfCondition` elemnt of AST.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LogicCondition {
    And,
    Or,
}

/// `ExpressionCondition` expression condition element of AST.
/// Used in `ExpressionLogicCondition` for `IfCondition` declaration.
/// It contains condition between twe expressions.
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionCondition<'a> {
    /// Left expression
    pub left: Expression<'a>,
    /// Condition between left and right expressions
    pub condition: Condition,
    /// Righ expression
    pub right: Expression<'a>,
}

/// # Logic expression condition
/// `ExpressionLogicCondition` expression logic condition used for
/// `IfCondition` declaration.
/// Expression logic condition can contains left and optional right
/// parts. Right part represent logic condition (like `or`, `and`) to
/// other `ExpressionLogicCondition`. So finally ir can represent tree
/// expressions logic conditions.
#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionLogicCondition<'a> {
    /// Left expression condition
    pub left: ExpressionCondition<'a>,
    /// Optional right side contain logic operation to other `ExpressionLogicCondition`
    pub right: Option<(LogicCondition, Box<ExpressionLogicCondition<'a>>)>,
}

/// `IfCondition` if-condition control flow element of AST.
/// One of the basic control flow. Basic entities:
/// - single expression condition
/// - logic expression condition tree
#[derive(Debug, Clone, PartialEq)]
pub enum IfCondition<'a> {
    /// Single if condition based on expression
    Single(Expression<'a>),
    /// Logic expression condition tree
    Logic(ExpressionLogicCondition<'a>),
}

/// `IfStatement` if statement AST element.
/// Contains full representation of if statement with entities:
/// - if-condition
/// - If-body statement - body of if-condition success
/// - Ff-else-body statement - body of else-condition success
/// - Else-if-body statement - body of else-if-condition success
#[derive(Debug, Clone, PartialEq)]
pub struct IfStatement<'a> {
    /// If-condition
    pub condition: IfCondition<'a>,
    /// If-body statement - body of if-condition success
    pub body: IfBodyStatements<'a>,
    /// If-else-body statement - body of else-condition success
    pub else_statement: Option<IfBodyStatements<'a>>,
    /// Else-if-body statement - body of else-if-condition success
    pub else_if_statement: Option<Box<IfStatement<'a>>>,
}

impl GetLocation for IfStatement<'_> {
    fn location(&self) -> CodeLocation {
        // TODO
        CodeLocation::new(1, 0)
    }
}

/// `BodyStatement` one of the basic AST elements.
/// It's part of Function body.
#[derive(Debug, Clone, PartialEq)]
pub enum BodyStatement<'a> {
    /// Let-binding function declaration
    LetBinding(LetBinding<'a>),
    /// Binding function declaration
    Binding(Binding<'a>),
    /// Function call
    FunctionCall(FunctionCall<'a>),
    /// If-condition control flow statement
    If(IfStatement<'a>),
    /// Loop control flow statement
    Loop(Vec<LoopBodyStatement<'a>>),
    /// Expression statement
    Expression(Expression<'a>),
    /// Return statement
    Return(Expression<'a>),
}

/// `IfBodyStatement` statement of if-body elements tree of AST.
/// Used as body statement of If-control flow.
#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatement<'a> {
    LetBinding(LetBinding<'a>),
    Binding(Binding<'a>),
    FunctionCall(FunctionCall<'a>),
    If(IfStatement<'a>),
    Loop(Vec<LoopBodyStatement<'a>>),
    Return(Expression<'a>),
}

/// `IfLoopBodyStatement` statement of loop-if-body elements tree of AST.
/// Used as body statement of If-control flow in the `Loop` AST element.
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

/// `IfBodyStatements` set of elements in the AST, that represents
/// control flow: `if`, `loop`
#[derive(Debug, Clone, PartialEq)]
pub enum IfBodyStatements<'a> {
    If(Vec<IfBodyStatement<'a>>),
    Loop(Vec<IfLoopBodyStatement<'a>>),
}

/// `LoopBodyStatement` statement of loop-body elements tree of AST.
/// Used as body statement of loop-control flow.
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

/// `MainStatement` main AST statement for all elements.
#[derive(Debug, Clone, PartialEq)]
pub enum MainStatement<'a> {
    /// Import declarations
    Import(ImportPath<'a>),
    /// Constant declarations
    Constant(Constant<'a>),
    /// Type declaration
    Types(StructTypes<'a>),
    /// Function declaration and function body-statement
    Function(FunctionStatement<'a>),
}

/// # Main
/// Stack of `MainStatement` main AST elements. That gather
/// tries of AST, to represent full sort of source code.
pub type Main<'a> = Vec<MainStatement<'a>>;
