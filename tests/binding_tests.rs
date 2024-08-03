use crate::utils::{CustomExpression, CustomExpressionInstruction, SemanticTest};
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, GetName, Ident, ValueName};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::{ExpressionResult, ExpressionResultValue};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::{Binding, InnerValueName, PrimitiveValue, Value};
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn binding_transform() {
    let expr_ast = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::U64(3)),
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
    let _ = format!("{:?}", binding_ast.clone());
}

#[test]
fn binding_wrong_expression() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::ValueName(ast::ValueName::new(Ident::new("x"))),
        operation: None,
    };
    let binding = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ValueNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn binding_value_not_exist() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::I16(23)),
        operation: None,
    };
    let binding = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ValueNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn binding_value_not_mutable() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::U64(30)),
        operation: None,
    };
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: false,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr.clone()),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.is_empty_error());
    let inner_name: InnerValueName = "x.0".into();
    let val = Value {
        inner_name: inner_name.clone(),
        inner_type: Type::Primitive(PrimitiveTypes::U64),
        mutable: false,
        alloca: false,
        malloc: false,
    };

    let state = block_state.borrow().get_context().get();
    assert_eq!(state.len(), 1);
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
    let binding = ast::Binding {
        name: ValueName::new(Ident::new("x")),
        value: Box::new(expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ValueIsNotMutable),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn binding_value_found() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let expr = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::U64(30)),
        operation: None,
    };
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(expr.clone()),
    };
    t.state.let_binding(&let_binding, &block_state);
    assert!(t.is_empty_error());
    let inner_name: InnerValueName = "x.0".into();
    let val = Value {
        inner_name: inner_name.clone(),
        inner_type: Type::Primitive(PrimitiveTypes::U64),
        mutable: true,
        alloca: false,
        malloc: false,
    };

    let state = block_state.borrow().get_context().clone().get();
    assert_eq!(state.len(), 1);
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
    let new_expr = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::U64(100)),
        operation: None,
    };
    let binding = ast::Binding {
        name: ValueName::new(Ident::new("x")),
        value: Box::new(new_expr),
    };
    t.state.binding(&binding, &block_state);
    assert!(t.is_empty_error());
    let state = block_state.borrow().get_context().clone().get();
    assert_eq!(state.len(), 2);
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
    assert_eq!(
        state[1],
        SemanticStackContext::Binding {
            val: val.clone(),
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(100)),
            },
        }
    );
}
