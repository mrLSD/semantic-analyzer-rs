use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, GetName, MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS};
use semantic_analyzer::types::expression::ExpressionOperations;
use semantic_analyzer::types::{
    semantic::SemanticStackContext, Constant, ConstantName, ConstantValue, PrimitiveValue,
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
    let _cnt_expr = ast::ConstantExpression {
        value: ast::ConstantValue::Value(ast::PrimitiveValue::I8(10)),
        operation: None,
    };
}

#[test]
fn ast_expression_operations() {
    assert_eq!(ast::ExpressionOperations::Plus.priority(), 5);
    assert_eq!(ast::ExpressionOperations::Minus.priority(), 4);
    assert_eq!(ast::ExpressionOperations::Divide.priority(), 8);
    assert_eq!(
        ast::ExpressionOperations::Multiply.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(
        ast::ExpressionOperations::ShiftLeft.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(
        ast::ExpressionOperations::ShiftRight.priority(),
        MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS
    );
    assert_eq!(ast::ExpressionOperations::Or.priority(), 6);
    assert_eq!(ast::ExpressionOperations::Xor.priority(), 6);
    assert_eq!(ast::ExpressionOperations::And.priority(), 7);
    assert_eq!(ast::ExpressionOperations::Eq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::NotEq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::Great.priority(), 7);
    assert_eq!(ast::ExpressionOperations::GreatEq.priority(), 7);
    assert_eq!(ast::ExpressionOperations::LessEq.priority(), 7);

    let ex_op: ExpressionOperations = ast::ExpressionOperations::Plus.into();
    assert_eq!(ex_op, ExpressionOperations::Plus);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Minus.into();
    assert_eq!(ex_op, ExpressionOperations::Minus);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Multiply.into();
    assert_eq!(ex_op, ExpressionOperations::Multiply);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Divide.into();
    assert_eq!(ex_op, ExpressionOperations::Divide);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::ShiftLeft.into();
    assert_eq!(ex_op, ExpressionOperations::ShiftLeft);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::ShiftRight.into();
    assert_eq!(ex_op, ExpressionOperations::ShiftRight);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Or.into();
    assert_eq!(ex_op, ExpressionOperations::Or);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::And.into();
    assert_eq!(ex_op, ExpressionOperations::And);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Xor.into();
    assert_eq!(ex_op, ExpressionOperations::Xor);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Eq.into();
    assert_eq!(ex_op, ExpressionOperations::Eq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::NotEq.into();
    assert_eq!(ex_op, ExpressionOperations::NotEq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Great.into();
    assert_eq!(ex_op, ExpressionOperations::Great);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::Less.into();
    assert_eq!(ex_op, ExpressionOperations::Less);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::GreatEq.into();
    assert_eq!(ex_op, ExpressionOperations::GreatEq);
    let ex_op: ExpressionOperations = ast::ExpressionOperations::LessEq.into();
    assert_eq!(ex_op, ExpressionOperations::LessEq);
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
    assert!(t.state.errors.is_empty());
    let state = t.state.global.context.clone().get();
    assert_eq!(state.len(), 1);
    assert_eq!(
        state[0],
        SemanticStackContext::Constant {
            const_decl: const_statement.clone().into()
        }
    );
}
