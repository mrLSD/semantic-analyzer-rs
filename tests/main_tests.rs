use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, GetName, Ident};
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
        body: vec![body_return.clone()],
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

    assert_eq!(t.state.context.len(), 1);
    let st = t.state.context[0].borrow().context.clone().get();
    assert_eq!(st.len(), 1);
    assert_eq!(
        st[0],
        SemanticStackContext::ExpressionFunctionReturnWithLabel {
            expr_result: ExpressionResult {
                expr_type: Type::Primitive(PrimitiveTypes::Bool),
                expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Bool(true)),
            }
        }
    );
    let st_context = t.state.global.context.get();
    assert_eq!(st_context.len(), 3);
    assert_eq!(
        st_context[0],
        SemanticStackContext::Types {
            type_decl: ty.into()
        }
    );
    assert_eq!(
        st_context[1],
        SemanticStackContext::Constant {
            const_decl: constant1.into()
        }
    );
    assert_eq!(
        st_context[2],
        SemanticStackContext::FunctionDeclaration {
            fn_decl: fn1.into()
        }
    );
}
