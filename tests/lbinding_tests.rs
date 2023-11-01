use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::Binding;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn binding_transform() {
    let expr_ast = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(3)),
        operation: None,
    };
    let binding_ast = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(expr_ast.clone()),
    };
    assert_eq!(binding_ast.location(), CodeLocation::new(1, 0));
    assert_eq!(binding_ast.clone().name(), "x");

    let binding: Binding = binding_ast.clone().into();
    assert_eq!(binding.clone().to_string(), "x");
    assert_eq!(binding.value, Box::new(expr_ast.into()));
    // For grcov
    format!("{:?}", binding_ast.clone());
}

#[test]
fn binding_wrong_expression() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(ast::ValueName::new(Ident::new("x"))),
        operation: None,
    };
    let binding = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotFound));
}

#[test]
fn binding_value_not_exist() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I16(23)),
        operation: None,
    };
    let binding = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotFound));
}

#[test]
fn binding_value_not_mutable() {}

#[test]
fn binding_value_found() {}
