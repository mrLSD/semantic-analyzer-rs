use semantic_analyzer::ast;
use semantic_analyzer::ast::Ident;
use semantic_analyzer::semantic::State;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::semantic::SemanticStack;
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::{InnerValueName, LabelName, Value, ValueName};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
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

    let mut bst = BlockState::new(None);
    assert!(bst.values.is_empty());
    assert!(bst.inner_values_name.is_empty());
    assert!(bst.labels.is_empty());
    assert!(!bst.manual_return);
    assert!(bst.parent.is_none());
    assert!(bst.children.is_empty());
    assert_eq!(bst.context, SemanticStack::new());

    let bst1 = Rc::new(RefCell::new(BlockState::new(None)));
    let bst2 = Rc::new(RefCell::new(BlockState::new(Some(bst1.clone()))));
    bst.set_child(bst1.clone());
    assert_eq!(bst.children.len(), 1);

    assert_eq!(bst.children.len(), 1);
    assert!(bst2.borrow().parent.is_some());
    bst.set_return();
    assert!(bst.manual_return);

    let value_name: ValueName = "x1".into();
    let inner_value_name1: InnerValueName = value_name.into();
    assert_eq!(inner_value_name1.to_string(), "x1");
    let inner_value_name2: InnerValueName = "x2".into();
    assert_eq!(inner_value_name2.to_string(), "x2");
    let inner_value_name3: InnerValueName = String::from("x3").into();
    assert_eq!(inner_value_name3.to_string(), "x3");

    bst.set_inner_value_name(&inner_value_name1);
    bst.set_inner_value_name(&inner_value_name2);
    assert_eq!(bst.inner_values_name.len(), 2);
    assert!(bst.is_inner_value_name_exist(&inner_value_name1));
    assert!(bst.is_inner_value_name_exist(&inner_value_name2));
    assert!(!bst.is_inner_value_name_exist(&inner_value_name3));
    assert_eq!(
        bst.get_next_inner_name(&inner_value_name1).to_string(),
        "x1.0"
    );
    assert_eq!(
        bst.get_next_inner_name(&inner_value_name3).to_string(),
        "x3.0"
    );
    let inner_value_name4: InnerValueName = "x1.0".into();
    assert_eq!(
        bst.get_next_inner_name(&inner_value_name4).to_string(),
        "x1.1"
    );
    let pre_bst1 = BlockState::new(None);
    let pre_bst2 = BlockState::new(None);
    let bloc_states = vec![pre_bst1, pre_bst2];
    assert_eq!(bloc_states.len(), 2);

    // Set for parent
    bst1.borrow_mut().set_inner_value_name(&inner_value_name1);
    assert!(bst2.borrow().is_inner_value_name_exist(&inner_value_name1));
    bst2.borrow_mut().set_inner_value_name(&inner_value_name2);
    assert!(bst2.borrow().is_inner_value_name_exist(&inner_value_name2));
    assert!(bst1.borrow().is_inner_value_name_exist(&inner_value_name2));
    assert_eq!(
        bst2.borrow()
            .get_next_inner_name(&inner_value_name2)
            .to_string(),
        "x2.0"
    );
    let inner_value_name20 = "x2.0".into();
    bst2.borrow_mut().set_inner_value_name(&inner_value_name20);
    assert!(bst1.borrow().is_inner_value_name_exist(&inner_value_name20));
    assert_eq!(
        bst2.borrow()
            .get_next_inner_name(&inner_value_name2)
            .to_string(),
        "x2.1"
    );

    let lbl1: LabelName = String::from("lbl1").into();
    bst1.borrow_mut().set_label_name(&lbl1);
    assert!(bst1.borrow().is_label_name_exist(&lbl1));
    assert!(bst2.borrow().is_label_name_exist(&lbl1));
    assert_eq!(lbl1.to_string(), "lbl1");

    let lbl2 = bst1.borrow_mut().get_and_set_next_label(&lbl1);
    assert_eq!(lbl2.to_string(), "lbl1.0");

    let lbl: LabelName = String::from("lbl2").into();
    let lbl3 = bst2.borrow_mut().get_and_set_next_label(&lbl);
    assert!(bst1.borrow().is_label_name_exist(&lbl3));
    assert!(bst2.borrow().is_label_name_exist(&lbl3));
    assert_eq!(lbl3.to_string(), "lbl2");
    let lbl4 = bst2.borrow_mut().get_and_set_next_label(&lbl3);
    assert_eq!(lbl4.to_string(), "lbl2.0");
    let lbl5 = bst2.borrow_mut().get_and_set_next_label(&lbl4);
    assert_eq!(lbl5.to_string(), "lbl2.1");
    let lbl4 = bst2.borrow_mut().get_and_set_next_label(&lbl);
    assert_eq!(lbl4.to_string(), "lbl2.2");

    bst2.borrow_mut().set_return();
    assert!(bst1.borrow().manual_return);
    assert!(bst2.borrow().manual_return);

    let bst3 = Rc::new(RefCell::new(BlockState {
        values: HashMap::new(),
        inner_values_name: HashSet::new(),
        labels: HashSet::new(),
        manual_return: false,
        parent: None,
        children: vec![],
        context: SemanticStack::new(),
    }));
    bst3.borrow_mut().manual_return = true;
    let bst4 = BlockState::new(Some(bst3.clone()));
    assert!(bst4.parent.unwrap().borrow().manual_return);

    // Insert Value
    let vn1: ValueName = "x5".into();
    let val = Value {
        inner_name: inner_value_name20,
        inner_type: Type::Primitive(PrimitiveTypes::Bool),
        mutable: false,
        alloca: false,
        malloc: false,
    };
    bst1.borrow_mut().values.insert(vn1.clone(), val);
    assert_eq!(bst1.borrow().get_value_name(&vn1).is_some(), true);
    assert_eq!(bst2.borrow().get_value_name(&vn1).is_some(), true);
    assert!(bst3.borrow().get_value_name(&vn1).is_none());

    // Simple BodyState check
    let mut st1 = State::default();
    st1.context.push(bst1);
    st1.context.push(bst2);
    st1.context.push(bst3);
    assert_eq!(st1.context.len(), 3);
    let fs = ast::FunctionStatement {
        name: ast::FunctionName::new(Ident::new("fn1")),
        result_type: ast::Type::Primitive(ast::PrimitiveTypes::Bool),
        parameters: vec![],
        body: vec![],
    };
    st1.function_body(&fs);
    // Should contain error
    assert_eq!(st1.errors.len(), 1);
    assert_eq!(st1.context.len(), 4);
}
