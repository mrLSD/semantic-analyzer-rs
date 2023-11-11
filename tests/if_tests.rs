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
use semantic_analyzer::types::{LabelName, PrimitiveValue};
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
fn check_if_and_else_if_statement() {
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
    t.check_errors_len(1);
    t.check_error(StateErrorKind::IfElseDuplicated);
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

    let ctx = block_state.borrow().context.clone().get();
    assert_eq!(ctx.len(), 2);
    assert_eq!(
        ctx[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8,),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3,),),
            },
            label_if_begin: label_if_begin.clone().into(),
            label_if_end: label_if_end.into(),
        }
    );
    assert_eq!(
        ctx[1],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8,),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(3,),),
            },
            label_if_begin: label_if_begin.into(),
            label_if_end: label_if_else.into(),
        }
    );
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ValueNotFound));
}
