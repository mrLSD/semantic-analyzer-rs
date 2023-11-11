use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, CodeLocation, GetLocation, GetName, Ident};
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::ExpressionOperations;
use semantic_analyzer::types::{
    semantic::SemanticStackContext, Constant, ConstantExpression, ConstantName, ConstantValue,
    PrimitiveValue,
};

mod utils;

#[test]
fn const_name_ast_transform() {
    let const_name_ast = ast::ConstantName::new(ast::Ident::new("cnt1"));
    let const_name_into1: ConstantName = const_name_ast.clone().into();
    assert_eq!(const_name_ast.name(), const_name_into1.to_string());
    assert_eq!(const_name_ast.name(), "cnt1".to_string());
    let const_name_into2: ConstantName = String::from("cnt1").into();
    assert_eq!(const_name_into1, const_name_into2);
}

#[test]
fn const_ast_transform() {
    let const_name = ast::ConstantName::new(ast::Ident::new("cnt1"));
    let const_statement = ast::Constant {
        name: const_name.clone(),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
            operation: None,
        },
    };
    let const_semantic: Constant = const_statement.into();
    assert_eq!(const_semantic.name, const_name.into());
    assert_eq!(
        const_semantic.constant_type,
        ast::Type::Primitive(ast::PrimitiveTypes::I8).into()
    );
    let cnt_val: ConstantValue = ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)).into();
    assert_eq!(const_semantic.constant_value.value, cnt_val);
    assert_eq!(
        const_semantic.constant_value.value,
        ConstantValue::Value(PrimitiveValue::I8(10))
    );
    assert_eq!(const_semantic.constant_value.operation, None);
}

#[test]
fn const_value_ast_transform() {
    let cnt_val1: ConstantValue = ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)).into();
    assert_eq!(cnt_val1, ConstantValue::Value(PrimitiveValue::I8(10)));

    let const_name2 = ast::ConstantName::new(ast::Ident::new("cnt2"));
    let cnt_val2: ConstantValue = ast::ConstantValue::Constant(const_name2.clone()).into();
    assert_eq!(cnt_val2, ConstantValue::Constant(const_name2.into()));
}

#[test]
fn const_expr_ast_transform() {
    let cnt_expr_prev2 = ast::ConstantExpression {
        value: ast::ConstantValue::Constant(ast::ConstantName::new(Ident::new("cnt3"))),
        operation: None,
    };
    let cnt_expr_prev = ast::ConstantExpression {
        value: ast::ConstantValue::Value(ast::PrimitiveValue::I16(20)),
        operation: Some((
            ast::ExpressionOperations::Eq,
            Box::new(cnt_expr_prev2.clone()),
        )),
    };
    let cnt_expr_ast = ast::ConstantExpression {
        value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
        operation: Some((
            ast::ExpressionOperations::Eq,
            Box::new(cnt_expr_prev.clone()),
        )),
    };
    let cnt_expr: ConstantExpression = cnt_expr_ast.clone().into();
    let c_val: ConstantValue = cnt_expr_ast.value.clone().into();
    assert_eq!(cnt_expr.value, c_val);

    let expr_op: ExpressionOperations = ast::ExpressionOperations::Eq.into();
    let c_val2: ConstantValue = cnt_expr_prev.value.into();
    let cnt_expr_op = cnt_expr.operation.clone().unwrap();
    assert_eq!(cnt_expr_op.0, expr_op);
    assert_eq!(cnt_expr_op.1.value, c_val2);

    let cnt_expr_op2 = cnt_expr_op.clone().1.operation.unwrap();
    let c_val3: ConstantValue = cnt_expr_prev2.value.into();
    assert_eq!(cnt_expr_op2.0, expr_op);
    assert_eq!(cnt_expr_op2.1.value, c_val3);
    let mut t = SemanticTest::new();
    t.state
        .check_constant_value_expression(&cnt_expr_ast.operation);
}

#[test]
fn const_declaration() {
    let mut t = SemanticTest::new();
    let const_name = ast::ConstantName::new(ast::Ident::new("cnt1"));
    let const_statement = ast::Constant {
        name: const_name.clone(),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
            operation: None,
        },
    };
    t.state.constant(&const_statement);
    assert!(t.state.global.constants.contains_key(&const_name.into()));
    assert!(t.is_empty_error());

    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::Constant {
            const_decl: const_statement.clone().into()
        }
    );

    t.state.constant(&const_statement);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ConstantAlreadyExist),
        "Errors: {:?}",
        t.state.errors[0]
    );
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
}

#[test]
fn const_declaration_with_operations() {
    let mut t = SemanticTest::new();
    let const_name2 = ast::ConstantName::new(ast::Ident::new("cnt2"));

    let cnt_expr_prev2 = ast::ConstantExpression {
        value: ast::ConstantValue::Value(ast::PrimitiveValue::F32(1.1)),
        operation: None,
    };

    let cnt_expr_prev = ast::ConstantExpression {
        value: ast::ConstantValue::Constant(const_name2.clone()),
        operation: Some((ast::ExpressionOperations::Plus, Box::new(cnt_expr_prev2))),
    };

    // constant1
    let const_name1 = ast::ConstantName::new(ast::Ident::new("cnt1"));
    let const_statement = ast::Constant {
        name: const_name1.clone(),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
            operation: Some((ast::ExpressionOperations::Plus, Box::new(cnt_expr_prev))),
        },
    };
    assert_eq!(const_statement.location(), CodeLocation::new(1, 0));
    t.state.constant(&const_statement);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ConstantNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
    assert!(!t
        .state
        .global
        .constants
        .contains_key(&const_name1.clone().into()));
    t.clean_errors();

    // constant2
    let const_statement2 = ast::Constant {
        name: const_name2.clone(),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::I32),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
            operation: None,
        },
    };
    t.state.constant(&const_statement2);
    assert!(t.state.global.constants.contains_key(&const_name2.into()));

    t.state.constant(&const_statement);
    assert!(t.state.global.constants.contains_key(&const_name1.into()));
    assert!(t.is_empty_error());

    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 2);
    assert_eq!(
        state[0],
        SemanticStackContext::Constant {
            const_decl: const_statement2.into()
        }
    );
    assert_eq!(
        state[1],
        SemanticStackContext::Constant {
            const_decl: const_statement.into()
        }
    );
}
