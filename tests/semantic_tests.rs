use std::cell::RefCell;
use std::rc::Rc;
use z_rose::ast::{
    Expression, ExpressionValue, Ident, LetBinding, PrimitiveTypes, PrimitiveValue, Type, ValueName,
};
use z_rose::backend::dummy::Backend;
use z_rose::semantic::{State, ValueBlockState};

#[test]
fn expression_test() {
    let backend = Backend::new();
    let block_state = Rc::new(RefCell::new(ValueBlockState::new(None)));
    let mut s = State::new(backend);
    let src = Ident::new("x");
    let vn = ValueName::new(src);
    let expr = Expression {
        expression_value: ExpressionValue::ValueName(vn.clone()),
        operation: None,
    };

    let res = s.expression(&expr, &block_state);
    let err = res.unwrap_err();
    assert!(err.trace_state().contains("ValueNotFound"));

    let expr = Expression {
        expression_value: ExpressionValue::PrimitiveValue(PrimitiveValue::U32(10)),
        operation: None,
    };
    let res = s.expression(&expr, &block_state);
    assert!(res.is_ok());

    let lb = LetBinding {
        name: vn,
        value_type: Some(Type::Primitive(PrimitiveTypes::I32)),
        value: Box::new(expr),
    };
    let res = s.let_binding(&lb, &block_state);
    assert!(res.is_ok());
    println!("{:#?}", s.codegen.get_stack());
}
