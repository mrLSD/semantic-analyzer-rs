use crate::utils::{CustomExpression, CustomExpressionInstruction, SemanticTest};
use semantic_analyzer::ast::{self, GetName, Ident};
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::ExpressionOperations;
use semantic_analyzer::types::{
    expression::{ExpressionResult, ExpressionResultValue},
    semantic::SemanticStackContext,
    types::{PrimitiveTypes, Type},
    Function, PrimitiveValue, Value,
};

mod utils;

#[test]
fn main_run() {
    let mut t = SemanticTest::new();
    let imports: ast::ImportPath = vec![ast::ImportName::new(Ident::new("import1"))];
    let import_stm = ast::MainStatement::Import(imports);

    let constant1 = ast::Constant {
        name: ast::ConstantName::new(Ident::new("const1")),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::None),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Constant(ast::ConstantName::new(Ident::new("const2"))),
            operation: None,
        },
    };
    let constant_stm = ast::MainStatement::Constant(constant1.clone());

    let ty = ast::StructTypes {
        name: Ident::new("StructType"),
        attributes: vec![],
    };
    let ty_stm = ast::MainStatement::Types(ty.clone());

    let body_let_binding = ast::BodyStatement::LetBinding(ast::LetBinding {
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
    let body_binding = ast::BodyStatement::Binding(ast::Binding {
        name: ast::ValueName::new(Ident::new("x")),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
    });
    let body_fn_call = ast::BodyStatement::FunctionCall(ast::FunctionCall {
        name: ast::FunctionName::new(Ident::new("fn2")),
        parameters: vec![],
    });
    let body_if = ast::BodyStatement::If(ast::IfStatement {
        condition: ast::IfCondition::Single(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
            operation: None,
        }),
        body: ast::IfBodyStatements::If(vec![ast::IfBodyStatement::FunctionCall(
            ast::FunctionCall {
                name: ast::FunctionName::new(Ident::new("fn2")),
                parameters: vec![],
            },
        )]),
        else_statement: None,
        else_if_statement: None,
    });
    let body_loop = ast::BodyStatement::Loop(vec![ast::LoopBodyStatement::FunctionCall(
        ast::FunctionCall {
            name: ast::FunctionName::new(Ident::new("fn2")),
            parameters: vec![],
        },
    )]);
    let body_return = ast::BodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        vec![
            body_let_binding,
            body_binding,
            body_fn_call,
            body_if,
            body_loop,
            body_return.clone(),
        ],
    );
    let fn_stm = ast::MainStatement::Function(fn1.clone());

    let body_expr_return = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U16(23)),
        operation: None,
    });
    let fn2 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn2")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::U16),
        vec![body_expr_return],
    );
    let fn2_stm = ast::MainStatement::Function(fn2.clone());
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![import_stm, constant_stm, ty_stm, fn_stm, fn2_stm];
    // For grcov
    let _ = format!("{main_stm:#?}");
    t.state.run(&main_stm);
    assert!(t.is_empty_error());

    assert_eq!(
        t.state
            .global
            .constants
            .get(&constant1.clone().name().into())
            .unwrap(),
        &(constant1.clone().into())
    );
    assert_eq!(
        t.state.global.types.get(&ty.clone().name().into()).unwrap(),
        &Type::Struct(ty.clone().into())
    );
    let fn_state = t
        .state
        .global
        .functions
        .get(&fn1.clone().name().into())
        .unwrap();
    assert_eq!(fn_state.inner_name, fn1.name.clone().into());
    assert_eq!(fn_state.inner_type, fn1.result_type.clone().into());
    assert!(fn_state.parameters.is_empty());

    // Function body context
    assert_eq!(t.state.context.len(), 2);
    let ctx1 = t.state.context[0].borrow();
    assert_eq!(ctx1.children.len(), 2);
    assert!(ctx1.parent.is_none());
    let ctx2 = t.state.context[1].borrow();
    assert!(ctx2.children.is_empty());
    assert!(ctx2.parent.is_none());

    let ch_ctx1 = ctx1.children[0].clone();
    assert!(ch_ctx1.borrow().parent.is_some());
    assert!(ch_ctx1.borrow().children.is_empty());

    let ch_ctx2 = ctx1.children[1].clone();
    assert!(ch_ctx2.borrow().parent.is_some());
    assert!(ch_ctx2.borrow().children.is_empty());

    // Semantic stack context for the block fn2
    let st_ctx2 = ctx2.get_context().clone().get();
    assert_eq!(st_ctx2.len(), 1);
    assert_eq!(
        st_ctx2[0],
        SemanticStackContext::ExpressionFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::U16),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U16(23)),
            }
        }
    );

    let st_ch_ctx1 = ch_ctx1.borrow().get_context().clone().get();
    assert_eq!(st_ch_ctx1.len(), 5);
    assert_eq!(
        st_ch_ctx1[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_end").into(),
        }
    );
    assert_eq!(
        st_ch_ctx1[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin").into()
        }
    );
    assert_eq!(
        st_ch_ctx1[2],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn2").into(),
                inner_type: Type::Primitive(PrimitiveTypes::U16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 2,
        }
    );
    assert_eq!(
        st_ch_ctx1[3],
        SemanticStackContext::JumpTo {
            label: String::from("if_end").into()
        }
    );
    assert_eq!(
        st_ch_ctx1[4],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    let st_ch_ctx2 = ch_ctx2.borrow().get_context().clone().get();
    assert_eq!(st_ch_ctx2.len(), 5);
    assert_eq!(
        st_ch_ctx2[0],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        st_ch_ctx2[1],
        SemanticStackContext::SetLabel {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        st_ch_ctx2[2],
        SemanticStackContext::Call {
            call: Function {
                inner_name: String::from("fn2").into(),
                inner_type: Type::Primitive(PrimitiveTypes::U16),
                parameters: vec![],
            },
            params: vec![],
            register_number: 3,
        }
    );
    assert_eq!(
        st_ch_ctx2[3],
        SemanticStackContext::JumpTo {
            label: String::from("loop_begin").into()
        }
    );
    assert_eq!(
        st_ch_ctx2[4],
        SemanticStackContext::SetLabel {
            label: String::from("loop_end").into()
        }
    );

    // Global semantic stack context
    let st_global_context = t.state.global.context.get();
    assert_eq!(st_global_context.len(), 4);
    assert_eq!(
        st_global_context[0],
        SemanticStackContext::Types {
            type_decl: ty.into()
        }
    );
    assert_eq!(
        st_global_context[1],
        SemanticStackContext::Constant {
            const_decl: constant1.into()
        }
    );
    assert_eq!(
        st_global_context[2],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
    assert_eq!(
        st_global_context[3],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn2.into()
        }
    );

    // Semantic stack context for the block fn1
    let st_ctx1 = ctx1.get_context().clone().get();
    assert_eq!(st_ctx1.len(), 14);
    assert_eq!(
        st_ctx1[0],
        SemanticStackContext::LetBinding {
            let_decl: Value {
                inner_name: "x.0".into(),
                inner_type: Type::Primitive(PrimitiveTypes::Bool),
                mutable: true,
                alloca: false,
                malloc: false,
            },
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(false)),
            },
        }
    );
    assert_eq!(
        st_ctx1[1],
        SemanticStackContext::Binding {
            val: Value {
                inner_name: "x.0".into(),
                inner_type: Type::Primitive(PrimitiveTypes::Bool),
                mutable: true,
                alloca: false,
                malloc: false,
            },
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
        }
    );
    assert_eq!(
        st_ctx1[2],
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
    assert_eq!(st_ctx1[3], st_ch_ctx1[0]);
    assert_eq!(st_ctx1[4], st_ch_ctx1[1]);
    assert_eq!(st_ctx1[5], st_ch_ctx1[2]);
    assert_eq!(st_ctx1[6], st_ch_ctx1[3]);
    assert_eq!(st_ctx1[7], st_ch_ctx1[4]);
    assert_eq!(st_ctx1[8], st_ch_ctx2[0]);
    assert_eq!(st_ctx1[9], st_ch_ctx2[1]);
    assert_eq!(st_ctx1[10], st_ch_ctx2[2]);
    assert_eq!(st_ctx1[11], st_ch_ctx2[3]);
    assert_eq!(st_ctx1[12], st_ch_ctx2[4]);
    assert_eq!(
        st_ctx1[13],
        SemanticStackContext::ExpressionFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );
}

#[test]
fn double_return() {
    let mut t = SemanticTest::new();
    let body_return = ast::BodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let body_expr = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        vec![body_return, body_expr],
    );
    let fn_stm = ast::MainStatement::Function(fn1);
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.check_errors_len(2), "Errors: {:?}", t.state.errors.len());
    assert!(t.check_error_index(0, StateErrorKind::ForbiddenCodeAfterReturnDeprecated));
    assert!(t.check_error_index(1, StateErrorKind::ReturnAlreadyCalled));
}

#[test]
fn wrong_return_type() {
    let mut t = SemanticTest::new();
    let body_return = ast::BodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(10)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        vec![body_return],
    );
    let fn_stm = ast::MainStatement::Function(fn1);
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.check_errors_len(1), "Errors: {:?}", t.state.errors.len());
    assert!(
        t.check_error(StateErrorKind::WrongReturnType),
        "Errors: {:?}",
        t.state.errors[0]
    );
}

#[test]
fn expression_as_return() {
    let mut t = SemanticTest::new();
    let body_expr = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        vec![body_expr],
    );
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.is_empty_error());

    // Function body context
    assert_eq!(t.state.context.len(), 1);
    let ctx = t.state.context[0].borrow();
    assert!(ctx.children.is_empty());
    assert!(ctx.parent.is_none());

    // Semantic stack context for the block
    let st_ctx = ctx.get_context().clone().get();
    assert_eq!(st_ctx.len(), 1);
    assert_eq!(
        st_ctx[0],
        SemanticStackContext::ExpressionFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );

    // Global semantic stack context
    let st_global_context = t.state.global.context.get();
    assert_eq!(st_global_context.len(), 1);
    assert_eq!(
        st_global_context[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
}

#[test]
fn if_return_from_function() {
    let mut t = SemanticTest::new();
    let body_expr_return = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(5)),
        operation: None,
    });
    let if_expr_return = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(10)),
        operation: None,
    };
    let if_expr = ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    };
    let body_if = ast::BodyStatement::If(ast::IfStatement {
        condition: ast::IfCondition::Single(if_expr),
        body: ast::IfBodyStatements::If(vec![ast::IfBodyStatement::Return(if_expr_return)]),
        else_statement: None,
        else_if_statement: None,
    });
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![],
        ast::Type::Primitive(ast::PrimitiveTypes::I8),
        vec![body_if, body_expr_return],
    );
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.is_empty_error());

    // Function body context
    assert_eq!(t.state.context.len(), 1);
    let ctx = t.state.context[0].borrow();
    assert_eq!(ctx.children.len(), 1);
    assert!(ctx.parent.is_none());

    // Children block context
    let children_ctx = ctx.children[0].borrow();
    assert!(children_ctx.children.is_empty());
    assert!(children_ctx.parent.is_some());

    // Children semantic stack context for the block
    let st_children_ctx = children_ctx.get_context().clone().get();
    assert_eq!(st_children_ctx.len(), 4);
    assert_eq!(
        st_children_ctx[0],
        SemanticStackContext::IfConditionExpression {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            },
            label_if_begin: String::from("if_begin").into(),
            label_if_end: String::from("if_end").into(),
        }
    );
    assert_eq!(
        st_children_ctx[1],
        SemanticStackContext::SetLabel {
            label: String::from("if_begin").into()
        }
    );
    assert_eq!(
        st_children_ctx[2],
        SemanticStackContext::JumpFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(10)),
            }
        }
    );
    assert_eq!(
        st_children_ctx[3],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    // Semantic stack context for the block
    let st_ctx = ctx.get_context().get();
    assert_eq!(st_ctx.len(), 5);
    assert_eq!(st_ctx[0], st_children_ctx[0]);
    assert_eq!(st_ctx[1], st_children_ctx[1]);
    assert_eq!(st_ctx[2], st_children_ctx[2]);
    assert_eq!(st_ctx[3], st_children_ctx[3]);
    assert_eq!(
        st_ctx[4],
        SemanticStackContext::ExpressionFunctionReturnWithLabel {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(5)),
            }
        }
    );

    // Global semantic stack context
    let st_global_context = t.state.global.context.get();
    assert_eq!(st_global_context.len(), 1);
    assert_eq!(
        st_global_context[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
}

#[test]
fn function_args_and_let_binding() {
    let mut t = SemanticTest::new();
    let body_let_binding = ast::BodyStatement::LetBinding(ast::LetBinding {
        name: ast::ValueName::new(Ident::new("y")),
        mutable: true,
        value_type: Some(ast::Type::Primitive(ast::PrimitiveTypes::U64)),
        value: Box::new(ast::Expression {
            expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(23)),
            operation: Some((
                ast::ExpressionOperations::Plus,
                Box::new(ast::Expression {
                    expression_value: ast::ExpressionValue::ValueName(ast::ValueName::new(
                        Ident::new("x"),
                    )),
                    operation: None,
                }),
            )),
        }),
    });
    let body_expr_return = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::ValueName(ast::ValueName::new(Ident::new("y"))),
        operation: None,
    });

    let fn_param1 = ast::FunctionParameter {
        name: ast::ParameterName::new(Ident::new("x")),
        parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::U64),
    };
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![fn_param1.clone()],
        ast::Type::Primitive(ast::PrimitiveTypes::U64),
        vec![body_let_binding, body_expr_return],
    );
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.is_empty_error());

    assert_eq!(t.state.context.len(), 1);
    let ctx = t.state.context[0].borrow();
    assert!(ctx.children.is_empty());
    assert!(ctx.parent.is_none());

    let stm_ctx = ctx.get_context().get();
    let ty = Type::Primitive(PrimitiveTypes::U64);
    let value_x = Value {
        inner_name: "x".into(),
        inner_type: ty.clone(),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    let value_y = Value {
        inner_name: "y.0".into(),
        inner_type: ty.clone(),
        mutable: true,
        alloca: false,
        malloc: false,
    };
    assert_eq!(
        stm_ctx[0],
        SemanticStackContext::FunctionArg {
            value: value_x.clone(),
            func_arg: fn_param1.into(),
        }
    );
    assert_eq!(
        stm_ctx[1],
        SemanticStackContext::ExpressionValue {
            expression: value_x,
            register_number: 1,
        }
    );
    assert_eq!(
        stm_ctx[2],
        SemanticStackContext::ExpressionOperation {
            operation: ExpressionOperations::Plus,
            left_value: ExpressionResult {
                expr_type: ty.clone(),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::U64(23)),
            },
            right_value: ExpressionResult {
                expr_type: ty.clone(),
                expr_value: ExpressionResultValue::Register(1),
            },
            register_number: 2,
        }
    );
    assert_eq!(
        stm_ctx[3],
        SemanticStackContext::LetBinding {
            let_decl: value_y.clone(),
            expr_result: ExpressionResult {
                expr_type: ty.clone(),
                expr_value: ExpressionResultValue::Register(2),
            },
        }
    );
    assert_eq!(
        stm_ctx[4],
        SemanticStackContext::ExpressionValue {
            expression: value_y,
            register_number: 3,
        }
    );
    assert_eq!(
        stm_ctx[5],
        SemanticStackContext::ExpressionFunctionReturn {
            expr_result: ExpressionResult {
                expr_type: ty.clone(),
                expr_value: ExpressionResultValue::Register(3),
            }
        }
    );

    // Verify global entities
    let fn_state = t
        .state
        .global
        .functions
        .get(&fn1.clone().name().into())
        .unwrap();
    assert_eq!(fn_state.inner_name, fn1.name.clone().into());
    assert_eq!(fn_state.inner_type, fn1.result_type.clone().into());
    assert_eq!(fn_state.parameters.len(), 1);
    assert_eq!(fn_state.parameters[0], ty);
    let global_ctx = t.state.global.context.get();
    assert_eq!(global_ctx.len(), 1);
    assert_eq!(
        global_ctx[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
}

#[test]
fn function_args_duplication() {
    let mut t = SemanticTest::new();
    let body_expr_return = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::U64(10)),
        operation: None,
    });

    let fn_param1 = ast::FunctionParameter {
        name: ast::ParameterName::new(Ident::new("x")),
        parameter_type: ast::Type::Primitive(ast::PrimitiveTypes::U64),
    };
    let fn_param2 = fn_param1.clone();
    let fn1 = ast::FunctionStatement::new(
        ast::FunctionName::new(Ident::new("fn1")),
        vec![fn_param1.clone(), fn_param2.clone()],
        ast::Type::Primitive(ast::PrimitiveTypes::U64),
        vec![body_expr_return],
    );
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main<
        CustomExpressionInstruction,
        CustomExpression<CustomExpressionInstruction>,
    > = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::FunctionArgumentNameDuplicated));
}
