use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::{ExpressionResult, ExpressionResultValue};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::{InnerValueName, LetBinding, PrimitiveValue, Value};
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
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ValueNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
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
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::WrongLetType),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn let_binding_value_not_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(30)),
        operation: None,
    };
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.is_empty_error());
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);
    let inner_name: InnerValueName = "x.0".into();
    let val = Value {
        inner_name: inner_name.clone(),
        inner_type: Type::Primitive(PrimitiveTypes::U64),
        mutable: true,
        alloca: false,
        malloc: false,
    };
    assert_eq!(
        state[0],
        SemanticStackContext::LetBinding {
            let_decl: val.clone(),
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(30)),
            },
        }
    );
    assert!(block_state.borrow().inner_values_name.contains(&inner_name));
    assert_eq!(
        block_state.borrow().values.get(&("x".into())).unwrap(),
        &val
    );
}

#[test]
fn let_binding_value_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(30)),
        operation: None,
    };
    let inner_name: InnerValueName = "x.0".into();
    let val = Value {
        inner_name: inner_name.clone(),
        inner_type: Type::Primitive(PrimitiveTypes::U64),
        mutable: true,
        alloca: false,
        malloc: false,
    };
    block_state
        .borrow_mut()
        .values
        .insert("x".into(), val.clone());
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.is_empty_error());
    let state = block_state.borrow().context.clone().get();
    assert_eq!(state.len(), 1);

    let val2 = Value {
        inner_name: "x.1".into(),
        ..val.clone()
    };
    assert_eq!(
        state[0],
        SemanticStackContext::LetBinding {
            let_decl: val2.clone(),
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(30)),
            },
        }
    );
    assert!(block_state
        .borrow()
        .inner_values_name
        .contains(&val2.inner_name));
    assert_eq!(
        block_state.borrow().values.get(&("x".into())).unwrap(),
        &val2
    );
}
