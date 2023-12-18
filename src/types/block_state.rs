//! # Block State types
//! Block state Semantic types.

use super::semantic::SemanticStack;
use super::{Constant, Function, FunctionParameter, InnerValueName, LabelName, Value, ValueName};
use crate::types::condition::{Condition, LogicCondition};
use crate::types::expression::{ExpressionOperations, ExpressionResult};
use crate::types::semantic::SemanticContext;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

/// # Block state
/// - `values` - contains unique values map for current state but not unique
///   for parent states. The map contains key-value: `value_name` (unique
///   only for current state); and `Value` itself - value parameters.   
/// - `inner_values_name` - is entity that represent inner value name - it
///   can be different from `Value` name because it should be unique for all
///   parent states. For example, of 3 values with name `x`, inner value
///   name will be: [`x`, `x.0`, `x.1`]. It mean, inner value name can
///   contain `value counter` as end of the name.
/// - `labels` - labels set, for conditional operation. Unique for current
///   and all paren states.
/// - `last_register_number` - represent register counter for current and
///   all parent states for `Codegen`. Register represented as `u64` and
///   should be linearly incremented.
/// - `manual_return` - flag indicated, that return was invoked from
/// other state, for example: if-flow, loop-flow
/// - `parent` - represent parent states.  
#[derive(Debug)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct BlockState {
    /// State values
    pub values: HashMap<ValueName, Value>,
    /// Used to keep all names in the block state (and parent) as unique
    pub inner_values_name: HashSet<InnerValueName>,
    /// State labels for conditional operations
    pub labels: HashSet<LabelName>,
    /// Last register for unique register representation
    pub last_register_number: u64,
    /// Manual return from other states
    pub manual_return: bool,
    /// Parent state
    #[cfg_attr(
        feature = "codec",
        serde(
            serialize_with = "rc_serializer::serialize_option",
            deserialize_with = "rc_serializer::deserialize_option"
        )
    )]
    pub parent: Option<Rc<RefCell<BlockState>>>,
    /// children states
    #[cfg_attr(
        feature = "codec",
        serde(
            serialize_with = "rc_serializer::serialize_vec",
            deserialize_with = "rc_serializer::deserialize_vec"
        )
    )]
    pub children: Vec<Rc<RefCell<BlockState>>>,
    /// Semantic stack context for Block state
    context: SemanticStack,
}

impl BlockState {
    /// Init block state with optional `parent` state
    #[must_use]
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        // Get values from parent
        let (last_register_number, inner_values_name, labels, manual_return) =
            parent.clone().map_or_else(
                || (0, HashSet::new(), HashSet::new(), false),
                |p| {
                    let parent = p.borrow();
                    (
                        parent.last_register_number,
                        parent.inner_values_name.clone(),
                        parent.labels.clone(),
                        parent.manual_return,
                    )
                },
            );
        Self {
            values: HashMap::new(),
            children: vec![],
            inner_values_name,
            labels,
            last_register_number,
            manual_return,
            parent,
            context: SemanticStack::new(),
        }
    }

    #[must_use]
    pub fn get_context(&self) -> SemanticStack {
        self.context.clone()
    }

    /// Set `last_register_number` for current and parent states
    fn set_register(&mut self, last_register_number: u64) {
        self.last_register_number = last_register_number;
        // Set `last_register_number` for parents
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_register(last_register_number);
        }
    }

    /// Increment register
    pub fn inc_register(&mut self) {
        self.set_register(self.last_register_number + 1);
    }

    /// Get child Block state
    pub fn set_child(&mut self, child: Rc<RefCell<Self>>) {
        self.children.push(child);
    }

    /// Set value inner name to current state and parent states
    pub fn set_inner_value_name(&mut self, name: &InnerValueName) {
        self.inner_values_name.insert(name.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_inner_value_name(name);
        }
    }

    /// Check is `inner_value_name` exist in current and parent states
    #[must_use]
    pub fn is_inner_value_name_exist(&self, name: &InnerValueName) -> bool {
        if self.inner_values_name.contains(name) {
            return true;
        } else if let Some(parent) = &self.parent {
            return parent.borrow().is_inner_value_name_exist(name);
        }
        false
    }

    /// Get `Value` by value name from current state.
    /// If not found on current state - recursively find in parent states.
    #[must_use]
    pub fn get_value_name(&self, name: &ValueName) -> Option<Value> {
        if let Some(val) = self.values.get(name) {
            return Some(val.clone());
        } else if let Some(parent) = &self.parent {
            return parent.borrow().get_value_name(name);
        }
        None
    }

    /// Check is label name exist in current and parent states
    #[must_use]
    pub fn is_label_name_exist(&self, name: &LabelName) -> bool {
        if self.labels.contains(name) {
            return true;
        } else if let Some(parent) = &self.parent {
            return parent.borrow().is_label_name_exist(name);
        }
        false
    }

    /// Set label name to current and all parent states
    pub fn set_label_name(&mut self, name: &LabelName) {
        self.labels.insert(name.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_label_name(name);
        }
    }

    /// Set attribute counter - increment, if counter exist.
    #[must_use]
    pub fn set_attr_counter(val: &str) -> String {
        let val_attr: Vec<&str> = val.split('.').collect();
        if val_attr.len() == 2 {
            let i: u64 = val_attr[1].parse().unwrap_or_default();
            format!("{}.{:?}", val_attr[0], i + 1)
        } else {
            format!("{}.0", val_attr[0])
        }
    }

    /// Get and set next label for condition operations
    /// - If label doesn't exist in State - just insert to State and
    ///   self return
    /// - if label exists, get label counter
    pub fn get_and_set_next_label(&mut self, label: &LabelName) -> LabelName {
        // Check is label exists. If doesn't set it to State and return self
        if !self.is_label_name_exist(label) {
            self.set_label_name(label);
            return label.clone();
        }
        // If label exists, split and get number of label counter
        let name: LabelName = Self::set_attr_counter(&label.to_string()).into();
        if self.is_label_name_exist(&name) {
            self.get_and_set_next_label(&name)
        } else {
            self.set_label_name(&name);
            name
        }
    }

    /// Get next `inner_value_name` by name counter for current and
    /// parent states. The `inner_value_name` should always be unique.
    #[must_use]
    pub fn get_next_inner_name(&self, val: &InnerValueName) -> InnerValueName {
        // Increment inner value name counter for shadowed variable
        let name: InnerValueName = Self::set_attr_counter(&val.to_string()).into();
        if self.is_inner_value_name_exist(&name) {
            self.get_next_inner_name(&name)
        } else {
            name
        }
    }

    /// Set return status flag for current and parent states
    pub fn set_return(&mut self) {
        self.manual_return = true;
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_return();
        }
    }
}

impl SemanticContext for BlockState {
    fn expression_value(&mut self, expression: Value, register_number: u64) {
        self.context
            .expression_value(expression.clone(), register_number);
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .expression_value(expression, register_number);
        }
    }

    fn expression_const(&mut self, expression: Constant, register_number: u64) {
        self.context
            .expression_const(expression.clone(), register_number);
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .expression_const(expression, register_number);
        }
    }

    fn expression_struct_value(&mut self, expression: Value, index: u32, register_number: u64) {
        self.context
            .expression_struct_value(expression.clone(), index, register_number);
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .expression_struct_value(expression, index, register_number);
        }
    }

    fn expression_operation(
        &mut self,
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    ) {
        self.context.expression_operation(
            operation.clone(),
            left_value.clone(),
            right_value.clone(),
            register_number,
        );
        if let Some(parent) = &self.parent {
            parent.borrow_mut().expression_operation(
                operation,
                left_value,
                right_value,
                register_number,
            );
        }
    }

    fn call(&mut self, call: Function, params: Vec<ExpressionResult>, register_number: u64) {
        self.context
            .call(call.clone(), params.clone(), register_number);
        if let Some(parent) = &self.parent {
            parent.borrow_mut().call(call, params, register_number);
        }
    }

    fn let_binding(&mut self, let_decl: Value, expr_result: ExpressionResult) {
        self.context
            .let_binding(let_decl.clone(), expr_result.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().let_binding(let_decl, expr_result);
        }
    }

    fn binding(&mut self, val: Value, expr_result: ExpressionResult) {
        self.context.binding(val.clone(), expr_result.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().binding(val, expr_result);
        }
    }

    fn expression_function_return(&mut self, expr_result: ExpressionResult) {
        self.context.expression_function_return(expr_result.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().expression_function_return(expr_result);
        }
    }

    fn expression_function_return_with_label(&mut self, expr_result: ExpressionResult) {
        self.context
            .expression_function_return_with_label(expr_result.clone());
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .expression_function_return_with_label(expr_result);
        }
    }

    fn set_label(&mut self, label: LabelName) {
        self.context.set_label(label.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_label(label);
        }
    }

    fn jump_to(&mut self, label: LabelName) {
        self.context.jump_to(label.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().jump_to(label);
        }
    }

    fn if_condition_expression(
        &mut self,
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    ) {
        self.context.if_condition_expression(
            expr_result.clone(),
            label_if_begin.clone(),
            label_if_end.clone(),
        );
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .if_condition_expression(expr_result, label_if_begin, label_if_end);
        }
    }

    fn condition_expression(
        &mut self,
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
        register_number: u64,
    ) {
        self.context.condition_expression(
            left_result.clone(),
            right_result.clone(),
            condition.clone(),
            register_number,
        );
        if let Some(parent) = &self.parent {
            parent.borrow_mut().condition_expression(
                left_result,
                right_result,
                condition,
                register_number,
            );
        }
    }

    fn jump_function_return(&mut self, expr_result: ExpressionResult) {
        self.context.jump_function_return(expr_result.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().jump_function_return(expr_result);
        }
    }

    fn logic_condition(
        &mut self,
        logic_condition: LogicCondition,
        left_register_result: u64,
        right_register_result: u64,
        register_number: u64,
    ) {
        self.context.logic_condition(
            logic_condition.clone(),
            left_register_result,
            right_register_result,
            register_number,
        );
        if let Some(parent) = &self.parent {
            parent.borrow_mut().logic_condition(
                logic_condition,
                left_register_result,
                right_register_result,
                register_number,
            );
        }
    }

    fn if_condition_logic(
        &mut self,
        label_if_begin: LabelName,
        label_if_end: LabelName,
        result_register: u64,
    ) {
        self.context.if_condition_logic(
            label_if_begin.clone(),
            label_if_end.clone(),
            result_register,
        );
        if let Some(parent) = &self.parent {
            parent
                .borrow_mut()
                .if_condition_logic(label_if_begin, label_if_end, result_register);
        }
    }

    fn function_arg(&mut self, value: Value, func_arg: FunctionParameter) {
        self.context.function_arg(value.clone(), func_arg.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().function_arg(value, func_arg);
        }
    }
}

/// Custom Serde serializers for `Rc<RefCell<...>>`
#[cfg(feature = "codec")]
pub mod rc_serializer {
    use super::{Rc, RefCell};
    use serde::{ser::SerializeSeq, Deserialize, Deserializer, Serialize, Serializer};

    /// Serializer for `Rc<RefCell<T>`.
    #[allow(clippy::missing_errors_doc)]
    pub fn serialize<S, T>(rc: &Rc<RefCell<T>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize,
    {
        T::serialize(&*rc.borrow(), serializer)
    }

    /// Serializer for `Option<Rc<RefCell<T>>`.
    #[allow(clippy::missing_errors_doc)]
    pub fn serialize_option<S, T>(
        val: &Option<Rc<RefCell<T>>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize,
    {
        match val {
            Some(rc) => serialize(rc, serializer),
            None => serializer.serialize_none(),
        }
    }

    /// Serializer for `Vec<Rc<RefCell<T>>`.
    #[allow(clippy::missing_errors_doc)]
    pub fn serialize_vec<S, T>(val: &Vec<Rc<RefCell<T>>>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
        T: Serialize,
    {
        let mut seq = serializer.serialize_seq(Some(val.len()))?;
        for item in val {
            seq.serialize_element(&*item.borrow())?;
        }
        seq.end()
    }

    /// Deserializer for `Rc<RefCell<T>`
    #[allow(clippy::missing_errors_doc)]
    pub fn deserialize<'de, D, T>(deserializer: D) -> Result<Rc<RefCell<T>>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        let value = T::deserialize(deserializer)?;
        Ok(Rc::new(RefCell::new(value)))
    }

    /// Deserializer for `Option<Rc<RefCell<T>>`
    #[allow(clippy::missing_errors_doc)]
    pub fn deserialize_option<'de, D, T>(
        deserializer: D,
    ) -> Result<Option<Rc<RefCell<T>>>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        let opt = Option::<T>::deserialize(deserializer)?;
        Ok(opt.map(|value| Rc::new(RefCell::new(value))))
    }

    /// Deserializer for `Vec<Rc<RefCell<T>>`
    #[allow(clippy::missing_errors_doc)]
    // grcov-excl-start
    pub fn deserialize_vec<'de, D, T>(deserializer: D) -> Result<Vec<Rc<RefCell<T>>>, D::Error>
    where
        D: Deserializer<'de>,
        T: Deserialize<'de>,
    {
        let vec = Vec::<T>::deserialize(deserializer)?;
        Ok(vec
            .into_iter()
            .map(|item| Rc::new(RefCell::new(item)))
            .collect())
    } // grcov-excl-end
}
