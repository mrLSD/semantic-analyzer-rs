use crate::utils::{CustomExpression, CustomExpressionInstruction, SemanticTest};
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::FunctionCall;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn func_call_transform() {
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_call = ast::FunctionCall::<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > {
        name: fn_name.clone(),
        parameters: vec![],
    };
    let fn_call_into: FunctionCall = fn_call.clone().into();
    assert_eq!(fn_call.location(), CodeLocation::new(1, 0));
    assert_eq!(fn_call.name(), "fn1");
    assert_eq!(fn_call.name.to_string(), "fn1");
    assert_eq!(fn_call_into.to_string(), "fn1");
    assert!(fn_call_into.parameters.is_empty());

    let param1 = ast::Expression {
        expression_value: ast::ExpressionValue::<
            CustomExpressionInstruction,
            CustomExpression<CustomExpressionInstruction>,
        >::PrimitiveValue(ast::PrimitiveValue::Ptr),
        operation: None,
    };
    let fn_call2 = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![param1.clone()],
    };
    let fn_call_into2: FunctionCall = fn_call2.clone().into();
    assert_eq!(fn_call_into2.parameters.len(), 1);
    assert_eq!(fn_call_into2.parameters[0], param1.into());
}

#[test]
fn func_call_not_declared_func() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let param1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Ptr),
        operation: None,
    };
    let fn_call = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![param1.clone()],
    };
    let res = t.state.function_call(&fn_call, &block_state);
    assert!(res.is_none());
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::FunctionNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn func_call_wrong_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_decl_param1 = ast::FunctionParameter {
        name: ast::ParameterName::new(Ident::new("x")),
        parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let fn_statement = ast::FunctionStatement::new(
        fn_name.clone(),
        vec![fn_decl_param1],
        ast::Type::Primitive(ast::PrimitiveTypes::I16),
        vec![],
    );
    t.state.function_declaration(&fn_statement);
    assert!(t.is_empty_error());

    let param1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::F64(1.2)),
        operation: None,
    };
    let fn_call = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![param1.clone()],
    };
    let res = t.state.function_call(&fn_call, &block_state).unwrap();
    assert_eq!(res, Type::Primitive(PrimitiveTypes::I16));
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::FunctionParameterTypeWrong),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn func_call_declared_func() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_decl_param1 = ast::FunctionParameter {
        name: ast::ParameterName::new(Ident::new("x")),
        parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
    };
    let fn_statement = ast::FunctionStatement::new(
        fn_name.clone(),
        vec![fn_decl_param1],
        ast::Type::Primitive(ast::PrimitiveTypes::I32),
        vec![],
    );
    t.state.function_declaration(&fn_statement);
    assert!(t.is_empty_error());

    let param1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    };
    let fn_call = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![param1.clone()],
    };
    let res = t.state.function_call(&fn_call, &block_state).unwrap();
    assert_eq!(res, Type::Primitive(PrimitiveTypes::I32));
    assert!(t.is_empty_error());
}
