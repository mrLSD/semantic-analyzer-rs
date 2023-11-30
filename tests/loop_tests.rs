use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::Ident;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::condition::LoopBodyStatement;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::{ExpressionResult, ExpressionResultValue};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::{Function, PrimitiveValue, Value};
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
    // For grcov
    format!("{loop_stmts:#?}");
    for loop_stmt in loop_stmts {
        let loop_stmt_into: LoopBodyStatement = loop_stmt.into();
        // For grcov
        format!("{loop_stmt_into:#?}");
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

    let loop_stmt = [
        loop_body_let_binding,
        loop_body_binding,
        loop_body_fn_call,
        loop_body_if,
        loop_body_loop,
    ];
    t.state.loop_statement(&loop_stmt, &block_state);

    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().get_context().clone().get().len(), 0);
    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().children.len(), 1);

    let ctx = block_state.borrow().children[0].clone();
    assert!(ctx.borrow().parent.is_some());
    assert_eq!(ctx.borrow().children.len(), 2);

    let stm_ctx = ctx.borrow().get_context().clone().get();
    assert_eq!(stm_ctx.len(), 7);
    assert_eq!(
        stm_ctx[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        stm_ctx[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        stm_ctx[2],
        SemanticStackContext::LetBinding {
            let_decl: Value {
                inner_name: "x.0".into(),
                inner_type: Type::Primitive(PrimitiveTypes::Bool),
                mutable: true,
                alloca: false,
                malloc: false
            },
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(false)),
            },
        }
    );
    assert_eq!(
        stm_ctx[3],
        SemanticStackContext::Binding {
            val: Value {
                inner_name: "x.0".into(),
                inner_type: Type::Primitive(PrimitiveTypes::Bool),
                mutable: true,
                alloca: false,
                malloc: false
            },
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
        }
    );
    assert_eq!(
        stm_ctx[4],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn2").into(),
                inner_type: Type::Primitive(PrimitiveTypes::U16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 1,
        }
    );
    assert_eq!(
        stm_ctx[5],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        stm_ctx[6],
        SemanticStackContext::SetLabel {
            label: String::from("loop_end").into()
        }
    );

    let ch_ctx1 = ctx.borrow().children[0].clone();
    assert!(ch_ctx1.borrow().parent.is_some());
    assert!(ch_ctx1.borrow().children.is_empty());

    let ctx1 = ch_ctx1.borrow().get_context().clone().get();
    assert_eq!(ctx1.len(), 5);
    assert_eq!(
        ctx1[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_end").into()
        }
    );
    assert_eq!(
        ctx1[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin").into()
        }
    );
    assert_eq!(
        ctx1[2],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn2").into(),
                inner_type: Type::Primitive(PrimitiveTypes::U16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 2
        }
    );
    assert_eq!(
        ctx1[3],
        SemanticStackContext::JumpTo {
            label: String::from("if_end").into()
        }
    );
    assert_eq!(
        ctx1[4],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    let ch_ctx2 = ctx.borrow().children[1].clone();
    assert!(ch_ctx2.borrow().parent.is_some());
    assert!(ch_ctx2.borrow().children.is_empty());

    let ctx2 = ch_ctx2.borrow().get_context().clone().get();
    assert_eq!(ctx2.len(), 5);
    assert_eq!(
        ctx2[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin.0").into()
        }
    );
    assert_eq!(
        ctx2[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin.0").into()
        }
    );
    assert_eq!(
        ctx2[2],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn2").into(),
                inner_type: Type::Primitive(PrimitiveTypes::U16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 3
        }
    );
    assert_eq!(
        ctx2[3],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin.0").into()
        }
    );
    assert_eq!(
        ctx2[4],
        SemanticStackContext::SetLabel {
            label: String::from("loop_end.0").into()
        }
    );

    assert!(t.is_empty_error());
}

#[test]
fn loop_statements_instructions_after_return() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

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
    let loop_body_return = ast::LoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });

    let loop_stmt = [loop_body_return, loop_body_let_binding];
    t.state.loop_statement(&loop_stmt, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterReturnDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn loop_statements_instructions_after_break() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

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
    let loop_body_break = ast::LoopBodyStatement::Break;

    let loop_stmt = [loop_body_break, loop_body_let_binding];
    t.state.loop_statement(&loop_stmt, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterBreakDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn loop_statements_instructions_after_continue() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

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
    let loop_body_continue = ast::LoopBodyStatement::Continue;

    let loop_stmt = [loop_body_continue, loop_body_let_binding];
    t.state.loop_statement(&loop_stmt, &block_state);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterContinueDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn loop_statements_with_return_invocation() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

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
    let loop_body_return = ast::LoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });

    let loop_stmt = [loop_body_let_binding, loop_body_return];
    t.state.loop_statement(&loop_stmt, &block_state);

    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().get_context().clone().get().len(), 0);
    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().children.len(), 1);

    let ctx = block_state.borrow().children[0].clone();
    assert!(ctx.borrow().parent.is_some());
    assert!(ctx.borrow().children.is_empty());

    let stm_ctx = ctx.borrow().get_context().clone().get();
    assert_eq!(stm_ctx.len(), 4);
    assert_eq!(
        stm_ctx[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        stm_ctx[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        stm_ctx[2],
        SemanticStackContext::LetBinding {
            let_decl: Value {
                inner_name: "x.0".into(),
                inner_type: Type::Primitive(PrimitiveTypes::Bool),
                mutable: true,
                alloca: false,
                malloc: false
            },
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(false)),
            },
        }
    );
    assert_eq!(
        stm_ctx[3],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );

    assert!(t.is_empty_error());
}
