use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::semantic::SemanticStackContext;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn function_declaration() {
    let _block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(ast::Ident::new("fn1"));
    let fn_statement = ast::FunctionStatement {
        name: fn_name.clone(),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
        body: vec![],
    };
    t.state.function_declaration(&fn_statement);
    assert!(t.state.global.functions.contains_key(&fn_name.into()));
    assert!(!t.is_error());
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn_statement.clone().into()
        }
    );

    // Declaration with parameters
    let fn_name2 = ast::FunctionName::new(ast::Ident::new("fn2"));
    let fn_statement2 = ast::FunctionStatement {
        name: fn_name2.clone(),
        parameters: vec![ast::FunctionParameter {
            name: ast::ParameterName::new(ast::Ident::new("x")),
            parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::I64),
        }],
        ..fn_statement.clone()
    };
    t.state.function_declaration(&fn_statement2);
    assert!(t.state.global.functions.contains_key(&fn_name2.into()));
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 2);
    assert!(!t.is_error());
    assert_eq!(
        state[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn_statement.clone().into()
        }
    );
    assert_eq!(
        state[1],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn_statement2.clone().into()
        }
    );
}
