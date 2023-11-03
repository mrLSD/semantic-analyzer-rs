use semantic_analyzer::ast;
use semantic_analyzer::ast::Ident;
use semantic_analyzer::types::condition::{
    IfBodyStatement, IfBodyStatements, IfCondition, IfStatement,
};

mod utils;

#[test]
fn if_transform() {
    let if_condition_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::F32(1.2)),
        operation: None,
    };
    let if_condition = ast::IfCondition::Single(if_condition_expr.clone());
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: false,
        value_type: None,
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    };
    let if_body =
        ast::IfBodyStatements::If(vec![ast::IfBodyStatement::LetBinding(let_binding.clone())]);
    let if_statement1 = ast::IfStatement {
        condition: if_condition.clone(),
        body: if_body,
        else_statement: None,
        else_if_statement: None,
    };
    let if_statement1_into: IfStatement = if_statement1.into();
    assert_eq!(
        if_statement1_into.condition,
        IfCondition::Single(if_condition_expr.into())
    );
    assert_eq!(
        if_statement1_into.body,
        IfBodyStatements::If(vec![IfBodyStatement::LetBinding(let_binding.into())])
    );
    assert!(if_statement1_into.else_statement.is_none());
    assert!(if_statement1_into.else_if_statement.is_none());
}
