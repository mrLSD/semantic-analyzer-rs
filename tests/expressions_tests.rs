use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{GetName, MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS};
use semantic_analyzer::types::expression::ExpressionOperations;
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::{
    block_state::BlockState,
    error::StateErrorKind,
    expression::ExpressionResultValue,
    types::{PrimitiveTypes, Type},
    Constant, ConstantExpression, ConstantName, ConstantValue, PrimitiveValue, Value,
};
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn expression_value_name_not_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let src = ast::Ident::new("x");
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
