use semantic_analyzer::ast;
use semantic_analyzer::ast::Ident;
use semantic_analyzer::types::condition::LoopBodyStatement;

#[test]
fn loop_transform() {
    let let_binding = ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: false,
        value_type: None,
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    };
    let binding = ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    };
    let fn_call = ast::FunctionCall {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
    };
    let if_statement = ast::IfStatement {
        condition: ast::IfCondition::Single(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::F32(1.2)),
            operation: None,
        }),
        body: ast::IfBodyStatements::If(vec![]),
        else_statement: None,
        else_if_statement: None,
    };
    let loop_statement = ast::LoopBodyStatement::Break;
    let return_statement = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::F32(1.2)),
        operation: None,
    };
    let loop_stmts = vec![
        ast::LoopBodyStatement::LetBinding(let_binding.clone()),
        ast::LoopBodyStatement::Binding(binding.clone()),
        ast::LoopBodyStatement::FunctionCall(fn_call.clone()),
        ast::LoopBodyStatement::If(if_statement.clone()),
        ast::LoopBodyStatement::Loop(vec![loop_statement.clone()]),
        ast::LoopBodyStatement::Return(return_statement.clone()),
        ast::LoopBodyStatement::Break,
        ast::LoopBodyStatement::Continue,
    ];
    // For grcove
    format!("{loop_stmts:#?}");
    for loop_stmt in loop_stmts {
        let loop_stmt_into: LoopBodyStatement = loop_stmt.into();
        match loop_stmt_into {
            LoopBodyStatement::LetBinding(val) => assert_eq!(val, let_binding.clone().into()),
            LoopBodyStatement::Binding(val) => assert_eq!(val, binding.clone().into()),
            LoopBodyStatement::FunctionCall(val) => assert_eq!(val, fn_call.clone().into()),
            LoopBodyStatement::If(val) => assert_eq!(val, if_statement.clone().into()),
            LoopBodyStatement::Loop(val) => assert_eq!(val, vec![loop_statement.clone().into()]),
            LoopBodyStatement::Return(val) => assert_eq!(val, return_statement.clone().into()),
            LoopBodyStatement::Break => assert_eq!(
                LoopBodyStatement::Break,
                ast::LoopBodyStatement::Break.into()
            ),
            LoopBodyStatement::Continue => assert_eq!(
                LoopBodyStatement::Continue,
                ast::LoopBodyStatement::Continue.into()
            ),
        }
    }
}
