use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, GetName, Ident};
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::{
    expression::{ExpressionResult, ExpressionResultValue},
    semantic::SemanticStackContext,
    types::{PrimitiveTypes, Type},
    PrimitiveValue,
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

    let body_return = ast::BodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        body: vec![body_return],
    };
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main = vec![
        import_stm.clone(),
        constant_stm.clone(),
        ty_stm.clone(),
        fn_stm.clone(),
    ];
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
    let fn_sstate = t
        .state
        .global
        .functions
        .get(&fn1.clone().name().into())
        .unwrap();
    assert_eq!(fn_sstate.inner_name, fn1.name.clone().into());
    assert_eq!(fn_sstate.inner_type, fn1.result_type.clone().into());
    assert!(fn_sstate.parameters.is_empty());

    // Function body context
    assert_eq!(t.state.context.len(), 1);
    let ctx = t.state.context[0].borrow();
    assert!(ctx.children.is_empty());
    assert!(ctx.parent.is_none());

    // Semantic stack context for the block
    let st_ctx = ctx.context.clone().get();
    assert_eq!(st_ctx.len(), 1);
    assert_eq!(
        st_ctx[0],
        SemanticStackContext::ExpressionFunctionReturnWithLabel {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );

    // Global semantic stack context for
    let st_global_context = t.state.global.context.get();
    assert_eq!(st_global_context.len(), 3);
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
    let fn1 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        body: vec![body_return, body_expr],
    };
    let fn_stm = ast::MainStatement::Function(fn1);
    let main_stm: ast::Main = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::ReturnAlreadyCalled));
}

#[test]
fn wrong_return_type() {
    let mut t = SemanticTest::new();
    let body_return = ast::BodyStatement::Return(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::I8(10)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        body: vec![body_return],
    };
    let fn_stm = ast::MainStatement::Function(fn1);
    let main_stm: ast::Main = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.check_errors_len(1));
    assert!(t.check_error(StateErrorKind::WrongReturnType));
}

#[test]
fn expression_as_return() {
    let mut t = SemanticTest::new();
    let body_expr = ast::BodyStatement::Expression(ast::Expression {
        expression_value: ast::ExpressionValue::PrimitiveValue(ast::PrimitiveValue::Bool(true)),
        operation: None,
    });
    let fn1 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        body: vec![body_expr],
    };
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main = vec![fn_stm];
    t.state.run(&main_stm);
    assert!(t.is_empty_error());

    // Function body context
    assert_eq!(t.state.context.len(), 1);
    let ctx = t.state.context[0].borrow();
    assert!(ctx.children.is_empty());
    assert!(ctx.parent.is_none());

    // Semantic stack context for the block
    let st_ctx = ctx.context.clone().get();
    assert_eq!(st_ctx.len(), 1);
    assert_eq!(
        st_ctx[0],
        SemanticStackContext::ExpressionFunctionReturnWithLabel {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );

    // Global semantic stack context for
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
    let fn1 = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        parameters: vec![],
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::I8),
        body: vec![body_if, body_expr_return],
    };
    let fn_stm = ast::MainStatement::Function(fn1.clone());
    let main_stm: ast::Main = vec![fn_stm];
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
    let st_children_ctx = children_ctx.context.clone().get();
    assert_eq!(st_children_ctx.len(), 5);
    assert_eq!(
        st_children_ctx[0],
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
        SemanticStackContext::JumpTo {
            label: String::from("if_end").into()
        }
    );
    assert_eq!(
        st_children_ctx[4],
        SemanticStackContext::SetLabel {
            label: String::from("if_end").into()
        }
    );

    // Semantic stack context for the block
    let st_ctx = ctx.context.clone().get();
    assert_eq!(st_ctx.len(), 1);
    assert_eq!(
        st_ctx[0],
        SemanticStackContext::ExpressionFunctionReturnWithLabel {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::I8),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::I8(5)),
            }
        }
    );

    // Global semantic stack context for
    let st_global_context = t.state.global.context.get();
    assert_eq!(st_global_context.len(), 1);
    assert_eq!(
        st_global_context[0],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
}
