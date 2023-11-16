use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::Ident;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::condition::LoopBodyStatement;
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

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

#[test]
fn loop_statements() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    let fn2 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn2")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::U16),
        body: vec![ast::BodyStatement::Expression(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(23)),
            operation: None,
        })],
    };
    t.state.function_declaration(&fn2);

    let loop_body_let_binding = ast::LoopBodyStatement::LetBinding(ast::LetBinding {
        name: ast::ValueName::new(Ident::new("x")),
        mutable: true,
        value_type: None,
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(
                false,
            )),
            operation: None,
        }),
    });
    let loop_body_binding = ast::LoopBodyStatement::Binding(ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    });
    let loop_body_fn_call = ast::LoopBodyStatement::FunctionCall(ast::FunctionCall {
        name: ast::FunctionName::new(Ident::new("fn2")),
        parameters: vec![],
    });
    let loop_body_if = ast::LoopBodyStatement::If(ast::IfStatement {
        condition: ast::IfCondition::Single(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
        body: ast::IfBodyStatements::Loop(vec![ast::IfLoopBodyStatement::FunctionCall(
            ast::FunctionCall {
                name: ast::FunctionName::new(Ident::new("fn2")),
                parameters: vec![],
            },
        )]),
        else_statement: None,
        else_if_statement: None,
    });
    let loop_body_loop = ast::LoopBodyStatement::Loop(vec![ast::LoopBodyStatement::FunctionCall(
        ast::FunctionCall {
            name: ast::FunctionName::new(Ident::new("fn2")),
            parameters: vec![],
        },
    )]);
    let loop_body_return = ast::LoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let loop_body_break = ast::LoopBodyStatement::Break;
    let loop_body_continue = ast::LoopBodyStatement::Continue;

    let loop_stmt = [
        loop_body_let_binding,
        loop_body_binding,
        loop_body_fn_call,
        loop_body_if,
        loop_body_loop,
        loop_body_break,
        loop_body_continue,
        loop_body_return,
    ];
    t.state.loop_statement(&loop_stmt, &block_state);
    // println!("{:#?}", t.state);
    assert!(t.is_empty_error());
}
