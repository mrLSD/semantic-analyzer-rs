use std::cell::RefCell;
use std::rc::Rc;
use z_rose::ast::{
    Expression, ExpressionValue, Ident, LetBinding, PrimitiveTypes, PrimitiveValue, Type, ValueName,
};
use z_rose::backend::dummy::Backend;
use z_rose::semantic::{ExpressionResult, State};

#[test]
fn pure_expression_test() {
    let backend = Backend::new();
    let block_state = Rc::new(RefCell::new(ValueBlockState::new(None)));
    let mut s = State::new(backend);
    let src = Ident::new("x");
    let vn = ValueName::new(src);
    let expr1 = Expression {
        expression_value: ExpressionValue::ValueName(vn.clone()),
        operation: None,
    };

    let res = s.expression(&expr1, &block_state);
    let err = res.unwrap_err();
    assert!(err.trace_state().contains("ValueNotFound"));

    let expr2 = Expression {
        expression_value: ExpressionValue::PrimitiveValue(PrimitiveValue::I32(10)),
        operation: None,
    };
    let res = s.expression(&expr2, &block_state);
    assert!(res.is_ok());
    if let ExpressionResult::PrimitiveValue(val) = res.unwrap() {
        assert_eq!(val, PrimitiveValue::I32(10));
    } else {
        unreachable!();
    }

    let lb = LetBinding {
        name: vn,
        value_type: Some(Type::Primitive(PrimitiveTypes::I32)),
        value: Box::new(expr2),
    };
    let res = s.let_binding(&lb, &block_state);
    assert!(res.is_ok());
    let cs = s.codegen.get_stack();
    assert_eq!(cs.len(), 2);

    let res = s.expression(&expr1, &block_state);
    assert!(res.is_ok());
    if let ExpressionResult::Register(reg) = res.unwrap() {
        assert_eq!(reg, 1);
    } else {
        unreachable!();
    }
    let cs = s.codegen.get_stack();
    assert_eq!(cs.len(), 3);
}
