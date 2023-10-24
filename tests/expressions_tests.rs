use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{
    CodeLocation, GetLocation, GetName, Ident, MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS,
};
use semantic_analyzer::types::expression::{
    Expression, ExpressionOperations, ExpressionResult, ExpressionStructValue,
};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::{
    block_state::BlockState,
    error::StateErrorKind,
    expression::ExpressionResultValue,
    types::{PrimitiveTypes, Type},
    Constant, ConstantExpression, ConstantName, ConstantValue, Function, PrimitiveValue, Value,
    ValueName,
};
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn expression_ast_transform() {
    let value_name = ast::ValueName::new(Ident::new("x"));
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(value_name.clone()),
        operation: None,
    };
    assert_eq!(expr.location(), CodeLocation::new(1, 0));
    assert_eq!(value_name.location(), CodeLocation::new(1, 0));
    assert_eq!(value_name.name(), "x");
    let value_name_into: ValueName = value_name.into();
    assert_eq!(value_name_into.to_string(), "x");
    let expr_into: Expression = expr.into();
    assert_eq!(expr_into.expression_value.to_string(), "x");
    assert_eq!(expr_into.to_string(), "x");
    let value_name_into2: ValueName = String::from("x1").into();
    assert_eq!(value_name_into2.to_string(), "x1");
    let value_name_into3: ValueName = "x2".into();
    assert_eq!(value_name_into3.to_string(), "x2");
}

#[test]
fn expression_ast_transform_primitive_value_i8() {
    let val = ast::PrimitiveValue::I8(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::I8)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::I8(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_i16() {
    let val = ast::PrimitiveValue::I16(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::I16)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::I16(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_i32() {
    let val = ast::PrimitiveValue::I32(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::I32)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::I32(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_i64() {
    let val = ast::PrimitiveValue::I64(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::I64)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::I64(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_u8() {
    let val = ast::PrimitiveValue::U8(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::U8)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::U8(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_u16() {
    let val = ast::PrimitiveValue::U16(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::U16)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::U16(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_u32() {
    let val = ast::PrimitiveValue::U32(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::U32)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::U32(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_u64() {
    let val = ast::PrimitiveValue::U64(3);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::U64)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::U64(3), expr_val);
    assert_eq!(expr_val.to_string(), "3");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3");
}

#[test]
fn expression_ast_transform_primitive_value_f32() {
    let val = ast::PrimitiveValue::F32(3.1);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::F32)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::F32(3.1), expr_val);
    assert_eq!(expr_val.to_string(), "3.1");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3.1");
}

#[test]
fn expression_ast_transform_primitive_value_f64() {
    let val = ast::PrimitiveValue::F64(3.1);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::F64)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::F64(3.1), expr_val);
    assert_eq!(expr_val.to_string(), "3.1");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "3.1");
}

#[test]
fn expression_ast_transform_primitive_value_bool() {
    let val = ast::PrimitiveValue::Bool(true);
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::Bool)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::Bool(true), expr_val);
    assert_eq!(expr_val.to_string(), "true");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "true");
}

#[test]
fn expression_ast_transform_primitive_value_string() {
    let val = ast::PrimitiveValue::String("str".to_string());
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::String)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::String("str".to_string()), expr_val);
    assert_eq!(expr_val.to_string(), "str");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "str");
}

#[test]
fn expression_ast_transform_primitive_value_char() {
    let val = ast::PrimitiveValue::Char('a');
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::Char)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::Char('a'), expr_val);
    assert_eq!(expr_val.to_string(), "a");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "a");
}

#[test]
fn expression_ast_transform_primitive_value_ptr() {
    let val = ast::PrimitiveValue::Ptr;
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::Ptr)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::Ptr, expr_val);
    assert_eq!(expr_val.to_string(), "ptr");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "ptr");
}

#[test]
fn expression_ast_transform_primitive_value_none() {
    let val = ast::PrimitiveValue::None;
    assert_eq!(
        val.get_type(),
        ast::Type::Primitive(ast::PrimitiveTypes::None)
    );
    let expr_val: PrimitiveValue = val.clone().into();
    assert_eq!(PrimitiveValue::None, expr_val);
    assert_eq!(expr_val.to_string(), "None");
    let expr: Expression = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(val),
        operation: None,
    }
    .into();
    assert_eq!(expr.to_string(), "None");
}

#[test]
fn expression_ast_transform_primitive_struct_value() {
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("val")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr_struct_val: ExpressionStructValue = expr_struct_val.into();
    assert_eq!(expr_struct_val.to_string(), "val");
}

#[test]
fn expression_value_name_not_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let src = Ident::new("x");
    let value_name = ast::ValueName::new(src);
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(value_name),
        operation: None,
    };
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_error(StateErrorKind::ValueNotFound));
    let state = block_state.borrow().context.clone().get();
    assert!(state.is_empty());
    assert!(t.check_errors_len(1));
}

#[test]
fn expression_value_name_exists() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let value_name = ast::ValueName::new(ast::Ident::new("x"));
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(value_name.clone()),
        operation: None,
    };
    let ty = Type::Primitive(PrimitiveTypes::I8);
    let value = Value {
        inner_name: "x".into(),
        inner_type: ty.clone(),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert(value_name.into(), value.clone());
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(res.expr_value, ExpressionResultValue::Register);
    assert_eq!(res.expr_type, ty);
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::ExpressionValue { expression: value }
    );
    assert!(t.is_empty_error());
}

#[test]
fn expression_const_exists() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let src = ast::Ident::new("x");
    let const_name = ast::ValueName::new(src);
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(const_name.clone()),
        operation: None,
    };
    let ty = Type::Primitive(PrimitiveTypes::I8);
    let name: ConstantName = const_name.name().into();
    let value = Constant {
        name: name.clone(),
        constant_type: ty.clone(),
        constant_value: ConstantExpression {
            value: ConstantValue::Value(PrimitiveValue::I8(12)),
            operation: None,
        },
    };
    t.state.global.constants.insert(name, value.clone());
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(res.expr_value, ExpressionResultValue::Register);
    assert_eq!(res.expr_type, ty);
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::ExpressionConst { expression: value }
    );
    assert!(t.is_empty_error());
}

#[test]
fn expression_primitive_value() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I32(10)),
        operation: None,
    };
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(
        res.expr_value,
        ExpressionResultValue::PrimitiveValue(PrimitiveValue::I32(10))
    );
    assert_eq!(res.expr_type, Type::Primitive(PrimitiveTypes::I32));
    let state = block_state.borrow().context.clone().get();
    assert!(state.is_empty());
    assert!(t.is_empty_error());
}

#[test]
fn ast_expression_operations() {
    assert_eq!(ast::ExpressionOperations::Plus.priority(), 5);
    assert_eq!(ast::ExpressionOperations::Minus.priority(), 4);
    assert_eq!(ast::ExpressionOperations::Divide.priority(), 8);
    assert_eq!(
        ast::ExpressionOperations::Multiply.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(
        ast::ExpressionOperations::ShiftLeft.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(
        ast::ExpressionOperations::ShiftRight.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(ast::ExpressionOperations::Or.priority(), 6);
    assert_eq!(ast::ExpressionOperations::Xor.priority(), 6);
    assert_eq!(ast::ExpressionOperations::And.priority(), 7);
    assert_eq!(ast::ExpressionOperations::Eq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::NotEq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::Great.priority(), 7);
    assert_eq!(ast::ExpressionOperations::GreatEq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::LessEq.priority(), 7);

    let ex_op: ExpressionOperations = ast::ExpressionOperations::Plus.into();
    assert_eq!(ex_op, ExpressionOperations::Plus);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Minus.into();
    assert_eq!(ex_op, ExpressionOperations::Minus);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Multiply.into();
    assert_eq!(ex_op, ExpressionOperations::Multiply);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Divide.into();
    assert_eq!(ex_op, ExpressionOperations::Divide);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::ShiftLeft.into();
    assert_eq!(ex_op, ExpressionOperations::ShiftLeft);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::ShiftRight.into();
    assert_eq!(ex_op, ExpressionOperations::ShiftRight);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Or.into();
    assert_eq!(ex_op, ExpressionOperations::Or);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::And.into();
    assert_eq!(ex_op, ExpressionOperations::And);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Xor.into();
    assert_eq!(ex_op, ExpressionOperations::Xor);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Eq.into();
    assert_eq!(ex_op, ExpressionOperations::Eq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::NotEq.into();
    assert_eq!(ex_op, ExpressionOperations::NotEq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Great.into();
    assert_eq!(ex_op, ExpressionOperations::Great);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Less.into();
    assert_eq!(ex_op, ExpressionOperations::Less);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::GreatEq.into();
    assert_eq!(ex_op, ExpressionOperations::GreatEq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::LessEq.into();
    assert_eq!(ex_op, ExpressionOperations::LessEq);
}

#[test]
fn expression_struct_value_not_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("val")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotFound));
}

#[test]
fn expression_struct_value_wrong_struct_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("x")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let val = Value {
        inner_name: "x".into(),
        inner_type: Type::Primitive(PrimitiveTypes::Bool),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), val.clone());
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotStruct));
}

#[test]
fn expression_struct_value_type_not_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("x")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    let val = Value {
        inner_name: "x".into(),
        inner_type: Type::Struct(s_ty.into()),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), val.clone());
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::TypeNotFound));
}

#[test]
fn expression_struct_value_wrong_expression_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("x")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let s_attr = ast::StructType {
        attr_name: Ident::new("attr2"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    let val = Value {
        inner_name: "x".into(),
        inner_type: Type::Struct(s_ty.into()),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), val.clone());

    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    t.state.types(&s_ty);
    assert!(t.is_empty_error());
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::WrongExpressionType));
}

#[test]
fn expression_struct_value_wrong_struct_attribute() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("x")),
        attribute: ast::ValueName::new(Ident::new("attr2")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    let val = Value {
        inner_name: "x".into(),
        inner_type: Type::Struct(s_ty.into()),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), val.clone());

    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    t.state.types(&s_ty);
    assert!(t.is_empty_error());
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotStructField));
}

#[test]
fn expression_struct_value() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr_struct_val = ast::ExpressionStructValue {
        name: ast::ValueName::new(Ident::new("x")),
        attribute: ast::ValueName::new(Ident::new("attr1")),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::StructValue(expr_struct_val),
        operation: None,
    };
    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    let value = Value {
        inner_name: "x".into(),
        inner_type: Type::Struct(s_ty.into()),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), value.clone());

    let s_attr = ast::StructType {
        attr_name: Ident::new("attr1"),
        attr_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let s_ty = ast::StructTypes {
        name: "St".into(),
        attributes: vec![s_attr],
    };
    t.state.types(&s_ty);
    assert!(t.is_empty_error());
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert!(t.is_empty_error());
    assert_eq!(res.expr_value, ExpressionResultValue::Register);
    assert_eq!(res.expr_type, Type::Primitive(PrimitiveTypes::Bool));
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::ExpressionStructValue {
            expression: value,
            index: 0
        }
    );
}

#[test]
fn expression_func_call() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_call = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![],
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::FunctionCall(fn_call),
        operation: None,
    };
    let res = t.state.expression(&expr, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::FunctionNotFound));
    t.clean_errors();

    // Declare function
    let fn_statement = ast::FunctionStatement {
        name: fn_name.clone(),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Ptr),
        body: vec![],
    };
    t.state.function_declaration(&fn_statement);
    assert!(t.is_empty_error());

    let res = t.state.expression(&expr, &block_state).unwrap();
    assert!(t.is_empty_error());
    assert_eq!(
        res,
        ExpressionResult {
            expr_value: ExpressionResultValue::Register,
            expr_type: Type::Primitive(PrimitiveTypes::Ptr)
        }
    );
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::Call {
            call: Function {
                inner_name: fn_name.into(),
                inner_type: Type::Primitive(PrimitiveTypes::Ptr),
                parameters: vec![]
            },
            params: vec![],
        }
    );
}

#[test]
fn expression_sub_expression() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let sub_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U32(10)),
        operation: None,
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::Expression(Box::new(sub_expr)),
        operation: None,
    };
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert!(t.is_empty_error());
    assert_eq!(
        res,
        ExpressionResult {
            expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U32(10)),
            expr_type: Type::Primitive(PrimitiveTypes::U32)
        }
    );
    let state = block_state.borrow().context.clone().get();
    assert!(state.is_empty());
}

#[test]
fn expression_operation() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Char('b')),
        operation: None,
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Char('a')),
        operation: Some((ast::ExpressionOperations::Plus, Box::new(next_expr))),
    };
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert!(t.is_empty_error());
    assert_eq!(
        res,
        ExpressionResult {
            expr_type: Type::Primitive(PrimitiveTypes::Char),
            expr_value: ExpressionResultValue::Register
        }
    );
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::ExpressionOperation {
            operation: ExpressionOperations::Plus,
            left_value: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Char),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Char('a'))
            },
            right_value: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Char),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Char('b'))
            },
        }
    );
}

#[test]
fn expression_operation_wrong_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I64(10)),
        operation: None,
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(20)),
        operation: Some((ast::ExpressionOperations::Plus, Box::new(next_expr))),
    };
    let res = t.state.expression(&expr, &block_state);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::WrongExpressionType));
    assert!(res.is_none());
    let state = block_state.borrow().context.clone().get();
    assert!(state.is_empty());
}

#[test]
fn expression_multiple_operation1() {
    // Expression: (1+2)*3-4-5*6
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(1)),
        operation: None,
    };
    // Expr: 1 + 2
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(2)),
        operation: Some((ast::ExpressionOperations::Plus, Box::new(prev_next_expr))),
    };
    let next_expr4 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(6)),
        operation: None,
    };
    // Expr: 5 * 6
    let next_expr3 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(next_expr4))),
    };
    // Expr: 4 - 5 * 6
    let next_expr2 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(4)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(next_expr3))),
    };
    // Expr: 3 - 4 - 5 * 6
    let next_expr1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(3)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(next_expr2))),
    };
    // Expr (1 + 2) * 3 - 4 - 5 * 6
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::Expression(Box::new(prev_expr)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(next_expr1))),
    };
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(
        res,
        ExpressionResult {
            expr_type: Type::Primitive(PrimitiveTypes::U16),
            expr_value: ExpressionResultValue::Register
        }
    );
    // let state = block_state.borrow().context.clone().get();
    // println!("\nSTATE:{state:#?}");
    assert!(t.is_empty_error());
}

#[test]
fn expression_multiple_operation2() {
    // Expression: (100+2)*(3-4-5*6)
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(100)),
        operation: None,
    };
    // Expr: 100 + 2
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(2)),
        operation: Some((ast::ExpressionOperations::Plus, Box::new(prev_next_expr))),
    };
    let next_expr4 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(6)),
        operation: None,
    };
    // Expr: 5 * 6
    let next_expr3 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(next_expr4))),
    };
    // Expr: 4 - 5 * 6
    let next_expr2 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(4)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(next_expr3))),
    };
    // Expr: 3 - 4 - 5 * 6
    let next_expr1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(3)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(next_expr2))),
    };
    // Expr set brackets: (3 - 4 - 5 * 6)
    let next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::Expression(Box::new(next_expr1)),
        operation: None,
    };
    // Expr (100 + 2) * (3 - 4 - 5 * 6)
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::Expression(Box::new(prev_expr)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(next_expr))),
    };
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(
        res,
        ExpressionResult {
            expr_type: Type::Primitive(PrimitiveTypes::U16),
            expr_value: ExpressionResultValue::Register
        }
    );
    // let state = block_state.borrow().context.clone().get();
    // println!("\nSTATE:{state:#?}");
    assert!(t.is_empty_error());
}

#[test]
fn expression_multiple_operation_simple3() {
    // Expression: 100-5*6
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(6)),
        operation: None,
    };
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((
            ast::ExpressionOperations::Multiply,
            Box::new(prev_next_expr),
        )),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(100)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_expr))),
    };
    let _res = t.state.expression(&expr, &block_state).unwrap();
    // let state = block_state.borrow().context.clone().get();
    // println!("\nSTATE: {state:#?}");
    assert!(t.is_empty_error());
}

#[test]
fn expression_multiple_operation_simple4() {
    // Expression: 100-5*6
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(40)),
        operation: None,
    };
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_next_expr))),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(20)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(prev_expr))),
    };
    let _res = t.state.expression(&expr, &block_state).unwrap();
    // let state = block_state.borrow().context.clone().get();
    // println!("\nSTATE: {state:#?}");
    assert!(t.is_empty_error());
}

#[test]
fn expression_multiple_operation_simple5() {
    // Expression: 100-5*6
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr2 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: None,
    };
    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(40)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_next_expr2))),
    };
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_next_expr))),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(20)),
        operation: Some((ast::ExpressionOperations::Multiply, Box::new(prev_expr))),
    };
    let _res = t.state.expression(&expr, &block_state).unwrap();
    //let state = block_state.borrow().context.clone().get();
    //println!("\nSTATE: {state:#?}");
    assert!(t.is_empty_error());
}

#[test]
fn expression_multiple_operation_simple6() {
    // Expression: 100-5*6
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let prev_next_expr2 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(15)),
        operation: None,
    };

    let prev_next_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(6)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_next_expr2))),
    };
    let prev_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(5)),
        operation: Some((
            ast::ExpressionOperations::Multiply,
            Box::new(prev_next_expr),
        )),
    };
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(100)),
        operation: Some((ast::ExpressionOperations::Minus, Box::new(prev_expr))),
    };
    let _res = t.state.expression(&expr, &block_state).unwrap();
    // let state = block_state.borrow().context.clone().get();
    // println!("\nSTATE: {state:#?}");
    assert!(t.is_empty_error());
}
