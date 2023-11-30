use crate::utils::SemanticTest;
use semantic_analyzer::ast;
use semantic_analyzer::ast::{CodeLocation, GetLocation, Ident};
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::condition::{
    Condition, ExpressionCondition, ExpressionLogicCondition, IfBodyStatement, IfBodyStatements,
    IfCondition, IfLoopBodyStatement, IfStatement, LogicCondition,
};
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::{Expression, ExpressionResult, ExpressionResultValue};
use semantic_analyzer::types::semantic::SemanticStackContext;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::{Function, LabelName, PrimitiveValue, Value};
use std::cell::RefCell;
use std::rc::Rc;

mod utils;

#[test]
fn if_single_transform() {
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
    let if_statement2 = ast::IfStatement {
        condition: if_condition.clone(),
        body: ast::IfBodyStatements::If(vec![]),
        else_statement: None,
        else_if_statement: None,
    };
    let loop_statement = ast::LoopBodyStatement::Break;
    let return_statement = if_condition_expr.clone();
    let if_body = ast::IfBodyStatements::If(vec![
        ast::IfBodyStatement::LetBinding(let_binding.clone()),
        ast::IfBodyStatement::Binding(binding.clone()),
        ast::IfBodyStatement::FunctionCall(fn_call.clone()),
        ast::IfBodyStatement::If(if_statement2.clone()),
        ast::IfBodyStatement::Loop(vec![loop_statement.clone()]),
        ast::IfBodyStatement::Return(return_statement.clone()),
    ]);
    let mut if_statement1 = ast::IfStatement {
        condition: if_condition.clone(),
        body: if_body.clone(),
        else_statement: None,
        else_if_statement: None,
    };
    if_statement1.else_statement = Some(if_body.clone());
    if_statement1.else_if_statement = Some(Box::new(if_statement1.clone()));
    let if_statement1_into: IfStatement = if_statement1.clone().into();
    assert_eq!(
        if_statement1_into.condition,
        IfCondition::Single(if_condition_expr.clone().into())
    );

    let if_let_binding_into = IfBodyStatement::LetBinding(let_binding.into());
    let if_binding_into = IfBodyStatement::Binding(binding.into());
    let if_fn_call_into = IfBodyStatement::FunctionCall(fn_call.into());
    let if_if_statement2_into = IfBodyStatement::If(if_statement2.into());
    let loop_statement_into = IfBodyStatement::Loop(vec![loop_statement.into()]);
    let return_statement_into = IfBodyStatement::Return(return_statement.into());
    assert_eq!(
        if_statement1_into.body,
        IfBodyStatements::If(vec![
            if_let_binding_into.clone(),
            if_binding_into.clone(),
            if_fn_call_into.clone(),
            if_if_statement2_into.clone(),
            loop_statement_into.clone(),
            return_statement_into.clone()
        ])
    );
    assert_eq!(
        if_statement1_into.clone().else_statement.unwrap(),
        IfBodyStatements::If(vec![
            if_let_binding_into,
            if_binding_into,
            if_fn_call_into,
            if_if_statement2_into,
            loop_statement_into,
            return_statement_into
        ])
    );
    let if_compare1 = *if_statement1_into.clone().else_if_statement.unwrap();
    let if_compare2 = IfStatement {
        else_if_statement: None,
        ..if_statement1_into.clone()
    };
    assert_eq!(if_compare1, if_compare2);
    assert_eq!(if_statement1.location(), CodeLocation::new(1, 0));
    // For grcov
    format!("{if_statement1:?}");
}

#[test]
fn if_logic_transform() {
    let if_condition_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::F32(1.2)),
        operation: None,
    };
    let if_condition_expr_into: Expression = if_condition_expr.clone().into();
    let expr_cond1 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::Great,
        right: if_condition_expr.clone(),
    };
    let expr_cond1_into: ExpressionCondition = expr_cond1.into();
    assert_eq!(expr_cond1_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond1_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond1_into.condition, Condition::Great);

    let expr_cond2 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::Less,
        right: if_condition_expr.clone(),
    };
    let expr_cond2_into: ExpressionCondition = expr_cond2.into();
    assert_eq!(expr_cond2_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond2_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond2_into.condition, Condition::Less);

    let expr_cond3 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::Eq,
        right: if_condition_expr.clone(),
    };
    let expr_cond3_into: ExpressionCondition = expr_cond3.into();
    assert_eq!(expr_cond3_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond3_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond3_into.condition, Condition::Eq);

    let expr_cond4 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::GreatEq,
        right: if_condition_expr.clone(),
    };
    let expr_cond4_into: ExpressionCondition = expr_cond4.into();
    assert_eq!(expr_cond4_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond4_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond4_into.condition, Condition::GreatEq);

    let expr_cond5 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::LessEq,
        right: if_condition_expr.clone(),
    };
    let expr_cond5_into: ExpressionCondition = expr_cond5.into();
    assert_eq!(expr_cond5_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond5_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond5_into.condition, Condition::LessEq);

    let expr_cond6 = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::NotEq,
        right: if_condition_expr.clone(),
    };
    let expr_cond6_into: ExpressionCondition = expr_cond6.into();
    assert_eq!(expr_cond6_into.left, if_condition_expr_into.clone());
    assert_eq!(expr_cond6_into.right, if_condition_expr_into.clone());
    assert_eq!(expr_cond6_into.condition, Condition::NotEq);

    let logic_cond_and = ast::LogicCondition::And;
    let logic_cond_and_into: LogicCondition = logic_cond_and.into();
    assert_eq!(logic_cond_and_into, LogicCondition::And);

    let logic_cond_and = ast::LogicCondition::Or;
    let logic_cond_and_into: LogicCondition = logic_cond_and.into();
    assert_eq!(logic_cond_and_into, LogicCondition::Or);

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

    let if_statement2 = ast::IfStatement {
        condition: ast::IfCondition::Single(if_condition_expr.clone()),
        body: ast::IfBodyStatements::If(vec![]),
        else_statement: None,
        else_if_statement: None,
    };
    let loop_statement = ast::LoopBodyStatement::Break;
    let return_statement = if_condition_expr.clone();
    let if_loop_body = ast::IfBodyStatements::Loop(vec![
        ast::IfLoopBodyStatement::LetBinding(let_binding.clone()),
        ast::IfLoopBodyStatement::Binding(binding.clone()),
        ast::IfLoopBodyStatement::FunctionCall(fn_call.clone()),
        ast::IfLoopBodyStatement::If(if_statement2.clone()),
        ast::IfLoopBodyStatement::Loop(vec![loop_statement.clone()]),
        ast::IfLoopBodyStatement::Return(return_statement.clone()),
        ast::IfLoopBodyStatement::Break,
        ast::IfLoopBodyStatement::Continue,
    ]);

    let expr_cond = ast::ExpressionCondition {
        left: if_condition_expr.clone(),
        condition: ast::Condition::Eq,
        right: if_condition_expr.clone(),
    };
    let mut expr_logic_cond = ast::ExpressionLogicCondition {
        left: expr_cond.clone(),
        right: None,
    };
    expr_logic_cond.right = Some((ast::LogicCondition::And, Box::new(expr_logic_cond.clone())));
    let if_logic_condition = ast::IfCondition::Logic(expr_logic_cond.clone());

    let if_statement3 = ast::IfStatement {
        condition: if_logic_condition.clone(),
        body: if_loop_body,
        else_statement: None,
        else_if_statement: None,
    };
    // For grcov
    format!("{if_statement3:?}");
    let expr_logic_cond_into: ExpressionLogicCondition = expr_logic_cond.into();
    assert_eq!(
        expr_logic_cond_into.left,
        ExpressionCondition {
            left: if_condition_expr_into.clone(),
            condition: Condition::Eq,
            right: if_condition_expr_into,
        }
    );
    let if_statement3_into: IfStatement = if_statement3.clone().into();
    assert_eq!(
        if_statement3_into.condition,
        IfCondition::Logic(expr_logic_cond_into)
    );
    let if_let_binding_into = IfLoopBodyStatement::LetBinding(let_binding.into());
    let if_binding_into = IfLoopBodyStatement::Binding(binding.into());
    let if_fn_call_into = IfLoopBodyStatement::FunctionCall(fn_call.into());
    let if_statement2_into = IfLoopBodyStatement::If(if_statement2.into());
    let loop_statement_into = IfLoopBodyStatement::Loop(vec![loop_statement.into()]);
    let return_statement_into = IfLoopBodyStatement::Return(return_statement.into());
    assert_eq!(
        if_statement3_into.body,
        IfBodyStatements::Loop(vec![
            if_let_binding_into,
            if_binding_into,
            if_fn_call_into,
            if_statement2_into,
            loop_statement_into,
            return_statement_into,
            IfLoopBodyStatement::Break,
            IfLoopBodyStatement::Continue,
        ])
    );
    assert!(if_statement3_into.else_if_statement.is_none());
    assert!(if_statement3_into.else_if_statement.is_none());
}

#[test]
fn check_if_and_else_if_statement_duplicate() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    };

    let if_else_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr.clone()),
        body: ast::IfBodyStatements::If(vec![]),
        else_statement: None,
        else_if_statement: None,
    };
    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![]),
        else_statement: Some(ast::IfBodyStatements::If(vec![])),
        else_if_statement: Some(Box::new(if_else_stmt)),
    };
    t.state.if_condition(&if_stmt, &block_state, None, None);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::IfElseDuplicated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_condition_calculation_simple() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    // Simple without operations
    let condition1 = ast::IfCondition::Single(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(3)),
        operation: None,
    });
    let label_if_begin: LabelName = String::from("if_begin").into();
    let label_if_else: LabelName = String::from("if_else").into();
    let label_if_end: LabelName = String::from("if_end").into();
    t.state.if_condition_calculation(
        &condition1,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );

    // Simple with else without operations
    let condition2 = ast::IfCondition::Single(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(3)),
        operation: None,
    });
    t.state.if_condition_calculation(
        &condition2,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        true,
    );

    // Simple with wrong expression without operations
    let condition3 = ast::IfCondition::Single(ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(ast::ValueName::new(Ident::new("x"))),
        operation: None,
    });
    t.state.if_condition_calculation(
        &condition3,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        true,
    );

    let ctx = block_state.borrow().get_context().clone().get();
    assert_eq!(ctx.len(), 2);
    assert_eq!(
        ctx[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8,),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3,),),
            },
            label_if_begin: label_if_begin.clone(),
            label_if_end,
        }
    );
    assert_eq!(
        ctx[1],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8,),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3,),),
            },
            label_if_begin,
            label_if_end: label_if_else,
        }
    );
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ValueNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_condition_calculation_logic() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    let left_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(3)),
        operation: None,
    };
    let right_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(6)),
        operation: None,
    };
    let label_if_begin: LabelName = String::from("if_begin").into();
    let label_if_else: LabelName = String::from("if_else").into();
    let label_if_end: LabelName = String::from("if_end").into();

    // Logic: left == right
    let condition1 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: None,
    });
    t.state.if_condition_calculation(
        &condition1,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );

    // Logic else: left == right
    let condition2 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: None,
    });
    t.state.if_condition_calculation(
        &condition2,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        true,
    );

    // Logic condition: left == right
    let condition3 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: Some((
            ast::LogicCondition::Or,
            Box::new(ast::ExpressionLogicCondition {
                left: ast::ExpressionCondition {
                    left: left_expr.clone(),
                    condition: ast::Condition::Eq,
                    right: right_expr.clone(),
                },
                right: None,
            }),
        )),
    });
    t.state.if_condition_calculation(
        &condition3,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );

    let ctx = block_state.borrow().get_context().clone().get();
    assert_eq!(
        ctx[0],
        SemanticStackContext::ConditionExpression {
            left_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3))
            },
            right_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(6))
            },
            condition: Condition::Eq,
            register_number: 1,
        }
    );
    assert_eq!(
        ctx[1],
        SemanticStackContext::IfConditionLogic {
            label_if_begin: label_if_begin.clone(),
            label_if_end: label_if_end.clone(),
            result_register: 1
        }
    );
    assert_eq!(
        ctx[2],
        SemanticStackContext::ConditionExpression {
            left_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3))
            },
            right_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(6))
            },
            condition: Condition::Eq,
            register_number: 2,
        }
    );
    assert_eq!(
        ctx[3],
        SemanticStackContext::IfConditionLogic {
            label_if_begin: label_if_begin.clone(),
            label_if_end: label_if_else,
            result_register: 2
        }
    );
    assert_eq!(
        ctx[4],
        SemanticStackContext::ConditionExpression {
            left_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3))
            },
            right_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(6))
            },
            condition: Condition::Eq,
            register_number: 3,
        }
    );
    assert_eq!(
        ctx[5],
        SemanticStackContext::ConditionExpression {
            left_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3))
            },
            right_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(6))
            },
            condition: Condition::Eq,
            register_number: 4,
        }
    );
    assert_eq!(
        ctx[6],
        SemanticStackContext::LogicCondition {
            logic_condition: LogicCondition::Or,
            left_register_result: 3,
            right_register_result: 4,
            register_number: 5
        }
    );
    assert_eq!(
        ctx[7],
        SemanticStackContext::IfConditionLogic {
            label_if_begin,
            label_if_end,
            result_register: 5
        }
    );
    assert_eq!(ctx.len(), 8);
    assert!(t.is_empty_error());
}

#[test]
fn if_condition_when_left_expr_return_error() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    // Left expression return error - function not found
    let left_expr = ast::Expression {
        expression_value: ast::ExpressionValue::FunctionCall(ast::FunctionCall {
            name: ast::FunctionName::new(Ident::new("fn1")),
            parameters: vec![],
        }),
        operation: None,
    };
    let right_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(6)),
        operation: None,
    };
    let label_if_begin: LabelName = String::from("if_begin").into();
    let label_if_else: LabelName = String::from("if_else").into();
    let label_if_end: LabelName = String::from("if_end").into();

    let condition1 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: None,
    });
    t.state.if_condition_calculation(
        &condition1,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );
    assert!(t.check_errors_len(2), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error_index(0, StateErrorKind::FunctionNotFound),
        "Errors: {:?}",
        t.state.errors[0]
    );
    assert!(
        t.check_error_index(1, StateErrorKind::ConditionIsEmpty),
        "Errors: {:?}",
        t.state.errors[1]
    );
}

#[test]
fn if_condition_left_expr_and_right_expr_different_type() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    // Left expression return error - function not found
    let left_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I16(10)),
        operation: None,
    };
    let right_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(6)),
        operation: None,
    };
    let label_if_begin: LabelName = String::from("if_begin").into();
    let label_if_else: LabelName = String::from("if_else").into();
    let label_if_end: LabelName = String::from("if_end").into();

    let condition1 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: None,
    });
    t.state.if_condition_calculation(
        &condition1,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ConditionExpressionWrongType),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_condition_primitive_type_only_check() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();

    let type_decl = ast::StructTypes {
        name: Ident::new("type1"),
        attributes: vec![],
    };
    t.state.types(&type_decl.clone());

    let fn_name = ast::FunctionName::new(Ident::new("fn1"));
    let fn_statement = ast::FunctionStatement {
        name: fn_name.clone(),
        parameters: vec![],
        result_type: ast::Type::Struct(type_decl),
        body: vec![],
    };
    t.state.function_declaration(&fn_statement);

    // Left expression return error - function not found
    let left_expr = ast::Expression {
        expression_value: ast::ExpressionValue::FunctionCall(ast::FunctionCall {
            name: fn_name.clone(),
            parameters: vec![],
        }),
        operation: None,
    };
    let right_expr = ast::Expression {
        expression_value: ast::ExpressionValue::FunctionCall(ast::FunctionCall {
            name: fn_name,
            parameters: vec![],
        }),
        operation: None,
    };
    let label_if_begin: LabelName = String::from("if_begin").into();
    let label_if_else: LabelName = String::from("if_else").into();
    let label_if_end: LabelName = String::from("if_end").into();

    let condition1 = ast::IfCondition::Logic(ast::ExpressionLogicCondition {
        left: ast::ExpressionCondition {
            left: left_expr.clone(),
            condition: ast::Condition::Eq,
            right: right_expr.clone(),
        },
        right: None,
    });
    t.state.if_condition_calculation(
        &condition1,
        &block_state,
        &label_if_begin,
        &label_if_else,
        &label_if_end,
        false,
    );
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ConditionExpressionNotSupported),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn else_if_statement() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };
    let if_else_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(2)),
        operation: None,
    };

    let if_else_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_else_expr),
        body: ast::IfBodyStatements::If(vec![ast::IfBodyStatement::Return(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(30)),
            operation: None,
        })]),
        else_statement: Some(ast::IfBodyStatements::Loop(vec![
            ast::IfLoopBodyStatement::Return(ast::Expression {
                expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(
                    10,
                )),
                operation: None,
            }),
        ])),
        else_if_statement: None,
    };
    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![ast::IfBodyStatement::Return(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(12)),
            operation: None,
        })]),
        else_statement: None,
        else_if_statement: Some(Box::new(if_else_stmt)),
    };
    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();
    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );
    assert!(t.is_empty_error());

    assert!(block_state.borrow().get_context().clone().get().is_empty());
    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().children.len(), 3);

    let ch_ctx1 = block_state.borrow().children[0].clone();
    assert!(ch_ctx1.borrow().parent.is_some());
    assert!(ch_ctx1.borrow().children.is_empty());

    let ch_ctx2 = block_state.borrow().children[1].clone();
    assert!(ch_ctx2.borrow().parent.is_some());
    assert!(ch_ctx2.borrow().children.is_empty());

    let ch_ctx3 = block_state.borrow().children[2].clone();
    assert!(ch_ctx3.borrow().parent.is_some());
    assert!(ch_ctx3.borrow().children.is_empty());

    let ctx1 = ch_ctx1.borrow().get_context().clone().get();
    assert_eq!(ctx1.len(), 5);
    assert_eq!(
        ctx1[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(1)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_else").into()
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
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(12)),
            }
        }
    );
    assert_eq!(
        ctx1[3],
        SemanticStackContext::SetLabel {
            label: String::from("if_else").into()
        }
    );
    assert_eq!(
        ctx1[4],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    let ctx2 = ch_ctx2.borrow().get_context().clone().get();
    assert_eq!(ctx2.len(), 4);
    assert_eq!(
        ctx2[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(2)),
            },
            label_if_begin: String::from("if_begin.0").into(),
            label_if_end: String::from("if_else.0").into()
        }
    );
    assert_eq!(
        ctx2[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin.0").into()
        }
    );
    assert_eq!(
        ctx2[2],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(30)),
            }
        }
    );
    assert_eq!(
        ctx2[3],
        SemanticStackContext::SetLabel {
            label: String::from("if_else.0").into()
        }
    );

    let ctx3 = ch_ctx3.borrow().get_context().clone().get();
    assert_eq!(ctx3.len(), 1);
    assert_eq!(
        ctx3[0],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(10)),
            }
        }
    );
}

#[test]
fn if_body_statements() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

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

    let if_body_let_binding = ast::IfBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_binding = ast::IfBodyStatement::Binding(ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    });
    let if_body_fn_call = ast::IfBodyStatement::FunctionCall(ast::FunctionCall {
        name: ast::FunctionName::new(Ident::new("fn2")),
        parameters: vec![],
    });
    let if_body_if = ast::IfBodyStatement::If(ast::IfStatement {
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
    let if_body_loop = ast::IfBodyStatement::Loop(vec![ast::LoopBodyStatement::FunctionCall(
        ast::FunctionCall {
            name: ast::FunctionName::new(Ident::new("fn2")),
            parameters: vec![],
        },
    )]);
    let if_body_return = ast::IfBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });

    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![
            if_body_let_binding,
            if_body_binding,
            if_body_fn_call,
            if_body_if,
            if_body_loop,
            if_body_return,
        ]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );
    assert!(t.is_empty_error());

    assert!(block_state.borrow().get_context().clone().get().is_empty());
    assert!(block_state.borrow().parent.is_none());
    assert_eq!(block_state.borrow().children.len(), 1);

    let ctx = block_state.borrow().children[0].clone();
    assert!(ctx.borrow().parent.is_some());
    assert_eq!(ctx.borrow().children.len(), 2);

    let stm_ctx = ctx.borrow().get_context().clone().get();
    assert_eq!(stm_ctx.len(), 7);
    assert_eq!(
        stm_ctx[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(1)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_end").into()
        }
    );
    assert_eq!(
        stm_ctx[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin").into()
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
            register_number: 1
        }
    );
    assert_eq!(
        stm_ctx[5],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );
    assert_eq!(
        stm_ctx[6],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    let ch_ctx1 = ctx.borrow().children[0].clone();
    assert!(ch_ctx1.borrow().parent.is_some());
    assert!(ch_ctx1.borrow().children.is_empty());

    let ctx1 = ch_ctx1.borrow().get_context().clone().get();
    assert_eq!(ctx1.len(), 4);
    assert_eq!(
        ctx1[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
            label_if_begin: String::from("if_begin.0").into(),
            label_if_end: String::from("if_end").into()
        }
    );
    assert_eq!(
        ctx1[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin.0").into()
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

    let ch_ctx2 = ctx.borrow().children[1].clone();
    assert!(ch_ctx2.borrow().parent.is_some());
    assert!(ch_ctx2.borrow().children.is_empty());

    let ctx2 = ch_ctx2.borrow().get_context().clone().get();
    assert_eq!(ctx2.len(), 5);
    assert_eq!(
        ctx2[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );

    assert_eq!(
        ctx2[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin").into()
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
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        ctx2[4],
        SemanticStackContext::SetLabel {
            label: String::from("loop_end").into()
        }
    );
}

#[test]
fn if_loop_body_statements() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

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
    let fn3 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn3")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::I16),
        body: vec![ast::BodyStatement::Expression(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I16(32)),
            operation: None,
        })],
    };
    t.state.function_declaration(&fn3);

    let if_body_let_binding = ast::IfLoopBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_binding = ast::IfLoopBodyStatement::Binding(ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    });
    let if_body_fn_call = ast::IfLoopBodyStatement::FunctionCall(ast::FunctionCall {
        name: ast::FunctionName::new(Ident::new("fn2")),
        parameters: vec![],
    });
    let if_body_if = ast::IfLoopBodyStatement::If(ast::IfStatement {
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
    let if_body_loop = ast::IfLoopBodyStatement::Loop(vec![ast::LoopBodyStatement::FunctionCall(
        ast::FunctionCall {
            name: ast::FunctionName::new(Ident::new("fn3")),
            parameters: vec![],
        },
    )]);
    let if_body_return = ast::IfLoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });

    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::Loop(vec![
            if_body_let_binding,
            if_body_binding,
            if_body_fn_call,
            if_body_if,
            if_body_loop,
            if_body_return,
        ]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );
    assert!(t.is_empty_error());

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
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U64),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(1)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_end").into()
        }
    );
    assert_eq!(
        stm_ctx[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin").into()
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
            register_number: 1
        }
    );
    assert_eq!(
        stm_ctx[5],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );
    assert_eq!(
        stm_ctx[6],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    let ch_ctx1 = ctx.borrow().children[0].clone();
    assert!(ch_ctx1.borrow().parent.is_some());
    assert!(ch_ctx1.borrow().children.is_empty());

    let ctx1 = ch_ctx1.borrow().get_context().clone().get();
    assert_eq!(ctx1.len(), 4);
    assert_eq!(
        ctx1[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
            label_if_begin: String::from("if_begin.0").into(),
            label_if_end: String::from("if_end").into()
        }
    );
    assert_eq!(
        ctx1[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin.0").into()
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

    let ch_ctx2 = ctx.borrow().children[1].clone();
    assert!(ch_ctx2.borrow().parent.is_some());
    assert!(ch_ctx2.borrow().children.is_empty());

    let ctx2 = ch_ctx2.borrow().get_context().clone().get();
    assert_eq!(ctx2.len(), 5);
    assert_eq!(
        ctx2[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        ctx2[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        ctx2[2],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn3").into(),
                inner_type: Type::Primitive(PrimitiveTypes::I16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 3
        }
    );
    assert_eq!(
        ctx2[3],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        ctx2[4],
        SemanticStackContext::SetLabel {
            label: String::from("loop_end").into()
        }
    );
}

#[test]
fn if_loop_body_instructions_after_return() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

    let if_body_let_binding = ast::IfLoopBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_return = ast::IfLoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::Loop(vec![if_body_return, if_body_let_binding]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );

    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterReturnDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn else_if_loop_body_instructions_after_return() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

    let if_body_let_binding = ast::IfLoopBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_return = ast::IfLoopBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::Loop(vec![if_body_return.clone()]),
        else_statement: Some(ast::IfBodyStatements::Loop(vec![
            if_body_return.clone(),
            if_body_let_binding,
        ])),
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );

    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterReturnDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_body_instructions_after_return() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

    let if_body_let_binding = ast::IfBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_return = ast::IfBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![if_body_return, if_body_let_binding]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );

    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterReturnDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_else_body_instructions_after_return() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };

    let if_body_let_binding = ast::IfBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_return = ast::IfBodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![if_body_return.clone()]),
        else_statement: Some(ast::IfBodyStatements::If(vec![
            if_body_return,
            if_body_let_binding,
        ])),
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );

    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterReturnDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_loop_body_instructions_after_break() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };
    let if_body_let_binding = ast::IfLoopBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_break = ast::IfLoopBodyStatement::Break;

    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::Loop(vec![if_body_break, if_body_let_binding]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterBreakDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn if_loop_body_instructions_after_continue() {
    let block_state = Rc::new(RefCell::new(BlockState::new(None)));
    let mut t = SemanticTest::new();
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(1)),
        operation: None,
    };
    let if_body_let_binding = ast::IfLoopBodyStatement::LetBinding(ast::LetBinding {
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
    let if_body_continue = ast::IfLoopBodyStatement::Continue;

    let label_loop_begin: LabelName = String::from("loop_begin").into();
    let label_loop_end: LabelName = String::from("loop_end").into();

    let if_stmt = ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::Loop(vec![if_body_continue, if_body_let_binding]),
        else_statement: None,
        else_if_statement: None,
    };

    t.state.if_condition(
        &if_stmt,
        &block_state,
        None,
        Some((&label_loop_begin, &label_loop_end)),
    );
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::ForbiddenCodeAfterContinueDeprecated),
        "Errors: {:?}",
        t.state.errors[0]
    );
}
