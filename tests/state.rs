use semantic_analyzer::semantic::State;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::semantic::SemanticStack;
use semantic_analyzer::types::{InnerValueName, ValueName};
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

    let mut bst = BlockState::new(None);
    assert!(bst.values.is_empty());
    assert!(bst.inner_values_name.is_empty());
    assert!(bst.labels.is_empty());
    assert!(!bst.manual_return);
    assert!(bst.parent.is_none());
    assert!(bst.children.is_empty());
    assert_eq!(bst.context, SemanticStack::new());

    let bst1 = BlockState::new(None);
    bst.set_child(Rc::new(RefCell::new(bst1)));
    assert_eq!(bst.children.len(), 1);
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
}
