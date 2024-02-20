use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, CodeLocation, GetLocation, Ident};
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::semantic::SemanticStackContext;

mod utils;

#[test]
fn function_transform_ast() {
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    assert_eq!(fn_name.location(), CodeLocation::new(1, 0));
    assert_eq!(fn_name.to_string(), "fn1");
}

#[test]
fn function_declaration_without_body() {
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_statement = ast::FunctionStatement::new(
        fn_name.clone(),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::I8),
        vec![],
    );
    t.state.function_declaration(&fn_statement);
    assert!(t.is_empty_error());
    assert!(t.state.global.functions.contains_key(&fn_name.into()));
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn_statement.clone().into()
        }
    );

    t.state.function_declaration(&fn_statement);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::FunctionAlreadyExist),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn function_declaration_wrong_type() {
    let mut t = SemanticTest::new();
    let fn_name = ast::FunctionName::new(Ident::new("fn2"));

    let type_decl = ast::StructTypes {
        name: Ident::new("type1"),
        attributes: vec![],
    };

    let fn_statement = ast::FunctionStatement::new(
        fn_name.clone(),
        vec![ast::FunctionParameter {
            name: ast::ParameterName::new(Ident::new("x")),
            parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::I64),
        }],
        ast::Type::Struct(type_decl.clone()),
        vec![],
    );
    t.state.function_declaration(&fn_statement);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::TypeNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );

    assert!(!t
        .state
        .global
        .functions
        .contains_key(&fn_name.clone().into()));
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 0);

    let fn_statement2 = ast::FunctionStatement::new(
        fn_statement.name,
        vec![ast::FunctionParameter {
            name: ast::ParameterName::new(Ident::new("x")),
            parameter_type: ast::Type::Struct(type_decl.clone()),
        }],
        ast::Type::Primitive(ast::PrimitiveTypes::I64),
        fn_statement.body,
    );
    t.state.function_declaration(&fn_statement2);
    assert!(t.check_errors_len(2), "Errors: {:?}", t.state.errors.len());
    assert!(t.check_error_index(1, StateErrorKind::TypeNotFound));
    t.clean_errors();

    t.state.types(&type_decl);
    assert!(t.is_empty_error());
    t.state.function_declaration(&fn_statement2);
    assert!(t.is_empty_error());
    assert!(t.state.global.functions.contains_key(&fn_name.into()));
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 2);
    assert_eq!(
        state[1],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn_statement2.into()
        }
    );
}
