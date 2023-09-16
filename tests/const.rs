use crate::utils::SemanticTest;
use semantic_analyzer::ast;

mod utils;

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
}
