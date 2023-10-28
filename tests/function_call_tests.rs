use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::FunctionCall;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn func_call_transform() {
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_call = ast::FunctionCall {
        name: fn_name.clone(),
        parameters: vec![],
    };
    let fn_call_into: FunctionCall = fn_call.clone().into();
    assert_eq!(fn_call.location(), CodeLocation::new(1, 0));
    assert_eq!(fn_call.name.to_string(), "fn1");
    assert_eq!(fn_call_into.to_string(), "fn1");
    assert!(fn_call_into.parameters.is_empty());

    let param1 = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Ptr),
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
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::FunctionNotFound));
}
