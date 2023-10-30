use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::LetBinding;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn let_binding_transform() {
    let expr_ast = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(3)),
        operation: None,
    };
    let let_binding_ast = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr_ast.clone()),
    };
    assert_eq!(let_binding_ast.location(), CodeLocation::new(1, 0));
    assert_eq!(let_binding_ast.clone().name(), "x");

    let let_binding: LetBinding = let_binding_ast.clone().into();
    assert_eq!(let_binding.clone().to_string(), "x");
    assert!(let_binding.mutable);
    assert_eq!(
        let_binding.value_type,
        Some(Type::Primitive(PrimitiveTypes::U64))
    );
    assert_eq!(let_binding.value, Box::new(expr_ast.into()));
    // For grcov
    format!("{:?}", let_binding_ast.clone());
}

#[test]
fn let_binding_wrong_expression() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(ast::ValueName::new(Ident::new("x"))),
        operation: None,
    };
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotFound));
}

#[test]
fn let_binding_wrong_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    };
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::WrongLetType));
}

#[test]
fn let_binding_value_not_found() {}

#[test]
fn let_binding_value_found() {}
