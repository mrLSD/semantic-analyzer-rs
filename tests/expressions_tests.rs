use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::semantic::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::{ExpressionResultValue, PrimitiveTypes, Type, Value};
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
}

#[test]
fn expression_value_name_exists() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let src = ast::Ident::new("x");
    let value_name = ast::ValueName::new(src);
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(value_name.clone()),
        operation: None,
    };
    let value = Value {
        inner_name: "x".into(),
        inner_type: Type::Primitive(PrimitiveTypes::I8),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert(value_name.into(), value);
    let res = t.state.expression(&expr, &block_state).unwrap();
    assert_eq!(res.expr_value, ExpressionResultValue::Register(1));
}
