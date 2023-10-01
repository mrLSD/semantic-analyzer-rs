use semantic_analyzer::ast::{self, Ident};
use semantic_analyzer::semantic::State;
use semantic_analyzer::types::{
    block_state::BlockState,
    semantic::SemanticStack,
    types::{PrimitiveTypes, Type},
    InnerValueName, LabelName, Value, ValueName,
};
use std::cell::RefCell;
use std::rc::Rc;

#[test]
fn state_init() {
    let st = State::default();
    assert!(st.global.types.is_empty());
    assert!(st.global.constants.is_empty());
    assert!(st.global.functions.is_empty());
    assert_eq!(st.global.context, SemanticStack::new());
    assert!(st.context.is_empty());
    assert!(st.errors.is_empty());
}

#[test]
fn state_block_state_count() {
    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    let mut st1 = State::default();
    st1.context.push(bst1);
    st1.context.push(bst2);
    assert_eq!(st1.context.len(), 2);
    let fs = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        parameters: vec![],
        body: vec![],
    };
    st1.function_body(&fs);
    // Should contain error
    assert_eq!(st1.errors.len(), 1);
    assert_eq!(st1.context.len(), 3);
}

#[test]
fn block_state_fields() {
    let mut bst = BlockState::new(None);
    assert!(bst.values.is_empty());
    assert!(bst.inner_values_name.is_empty());
    assert!(bst.labels.is_empty());
    assert!(!bst.manual_return);
    assert!(bst.parent.is_none());
    assert!(bst.children.is_empty());
    assert_eq!(bst.context, SemanticStack::new());
    bst.set_child(Rc::new(RefCell::new(BlockState::new(None))));
    assert_eq!(bst.children.len(), 1);

    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    bst1.borrow_mut().set_child(bst1.clone());
    bst2.borrow_mut().set_return();
    assert!(bst1.borrow().manual_return);
    assert!(bst2.borrow().manual_return);
}

#[test]
fn inner_value_name_transform() {
    let value_name: ValueName = "x1".into();
    let inner_value_name1: InnerValueName = value_name.into();
    assert_eq!(inner_value_name1.to_string(), "x1");
    let inner_value_name2: InnerValueName = "x2".into();
    assert_eq!(inner_value_name2.to_string(), "x2");
    let inner_value_name3: InnerValueName = String::from("x3").into();
    assert_eq!(inner_value_name3.to_string(), "x3");
}

#[test]
fn block_state_inner_value_name() {
    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    bst1.borrow_mut().set_child(bst1.clone());
    assert!(bst2.borrow().parent.is_some());
    assert_eq!(bst1.borrow().children.len(), 1);

    let inner_value_name1: InnerValueName = "x1".into();
    let inner_value_name2: InnerValueName = "x2".into();
    let inner_value_name3: InnerValueName = "x3".into();

    bst1.borrow_mut().set_inner_value_name(&inner_value_name1);
    bst2.borrow_mut().set_inner_value_name(&inner_value_name2);

    assert_eq!(bst1.borrow().inner_values_name.len(), 2);
    assert!(bst1.borrow().is_inner_value_name_exist(&inner_value_name1));
    assert!(bst1.borrow().is_inner_value_name_exist(&inner_value_name2));
    assert!(!bst1.borrow().is_inner_value_name_exist(&inner_value_name3));

    assert_eq!(bst2.borrow().inner_values_name.len(), 1);
    assert!(bst2.borrow().is_inner_value_name_exist(&inner_value_name1));
    assert!(bst2.borrow().is_inner_value_name_exist(&inner_value_name2));
    assert!(!bst2.borrow().is_inner_value_name_exist(&inner_value_name3));

    assert_eq!(
        bst1.borrow()
            .get_next_inner_name(&inner_value_name1)
            .to_string(),
        "x1.0"
    );
    assert_eq!(
        bst2.borrow()
            .get_next_inner_name(&inner_value_name1)
            .to_string(),
        "x1.0"
    );
    assert_eq!(
        bst1.borrow()
            .get_next_inner_name(&inner_value_name3)
            .to_string(),
        "x3.0"
    );
    let inner_value_name3_0: InnerValueName = "x3.0".into();
    assert_eq!(
        bst1.borrow()
            .get_next_inner_name(&inner_value_name3_0)
            .to_string(),
        "x3.1"
    );

    let inner_value_name2_0 = "x2.0".into();
    bst2.borrow_mut().set_inner_value_name(&inner_value_name2_0);
    assert!(bst2
        .borrow()
        .is_inner_value_name_exist(&inner_value_name2_0));
    assert_eq!(
        bst1.borrow()
            .get_next_inner_name(&inner_value_name2)
            .to_string(),
        "x2.1"
    );
    assert_eq!(
        bst2.borrow()
            .get_next_inner_name(&inner_value_name2)
            .to_string(),
        "x2.1"
    );
}

#[test]
fn block_state_label_name() {
    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    bst1.borrow_mut().set_child(bst1.clone());

    let lbl1: LabelName = String::from("lbl1").into();
    let lbl2: LabelName = String::from("lbl2").into();
    assert_eq!(lbl1.to_string(), "lbl1");
    bst1.borrow_mut().set_label_name(&lbl1);
    bst2.borrow_mut().set_label_name(&lbl2);
    assert!(bst1.borrow().is_label_name_exist(&lbl1));
    assert!(bst2.borrow().is_label_name_exist(&lbl1));
    assert!(bst1.borrow().is_label_name_exist(&lbl2));
    assert!(bst2.borrow().is_label_name_exist(&lbl2));

    let lbl3 = bst1
        .borrow_mut()
        .get_and_set_next_label(&String::from("lbl3").into());
    assert_eq!(lbl3.to_string(), "lbl3");

    let lbl1_0 = bst1.borrow_mut().get_and_set_next_label(&lbl1);
    assert_eq!(lbl1_0.to_string(), "lbl1.0");

    let lbl1_1 = bst2.borrow_mut().get_and_set_next_label(&lbl1);
    assert_eq!(lbl1_1.to_string(), "lbl1.1");
    let lbl1_2 = bst2.borrow_mut().get_and_set_next_label(&lbl1_1);
    assert_eq!(lbl1_2.to_string(), "lbl1.2");
    let lbl1_3 = bst2.borrow_mut().get_and_set_next_label(&lbl1);
    assert_eq!(lbl1_3.to_string(), "lbl1.3");
}

#[test]
fn block_state_value() {
    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    let bst3 = Rc::new(RefCell::new(BlockState::new(None)));
    bst1.borrow_mut().set_child(bst1.clone());

    // Insert Value
    let vn1: ValueName = "x5".into();
    let val = Value {
        inner_name: "x5.0".into(),
        inner_type: Type::Primitive(PrimitiveTypes::Bool),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    bst1.borrow_mut().values.insert(vn1.clone(), val.clone());
    assert_eq!(bst1.borrow().get_value_name(&vn1).unwrap(), val.clone());
    assert_eq!(bst2.borrow().get_value_name(&vn1).unwrap(), val);
    assert!(bst3.borrow().get_value_name(&vn1).is_none());
}
