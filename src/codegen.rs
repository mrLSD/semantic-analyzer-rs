use crate::types::{Condition, ExpressionResult, LabelName, LogicCondition};

#[allow(dead_code, clippy::module_name_repetitions)]
#[derive(Debug, Default, Clone, PartialEq)]
pub struct CodegenStack(Vec<StackKind>);

impl CodegenStack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, value: StackKind) {
        self.0.push(value);
    }
}

impl Codegen for CodegenStack {
    fn jump_function_return(&mut self, expr_result: &ExpressionResult) {
        self.push(StackKind::JumpFunctionReturn {
            expr_result: expr_result.clone(),
        });
    }

    fn logic_condition(
        &mut self,
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: &LogicCondition,
        register_number: u64,
    ) {
        self.push(StackKind::LogicCondition {
            left_condition_register,
            right_condition_register,
            logic_condition: logic_condition.clone(),
            register_number,
        });
    }

    fn if_condition_logic(
        &mut self,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
        register_number: u64,
    ) {
        self.push(StackKind::IfConditionLogic {
            label_if_begin: label_if_begin.clone(),
            label_if_end: label_if_end.clone(),
            register_number,
        });
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum StackKind {
    JumpFunctionReturn {
        expr_result: ExpressionResult,
    },
    LogicCondition {
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: LogicCondition,
        register_number: u64,
    },
    IfConditionLogic {
        label_if_begin: LabelName,
        label_if_end: LabelName,
        register_number: u64,
    },
}

pub trait Codegen {
    fn jump_function_return(&mut self, expr_result: &ExpressionResult);
    fn logic_condition(
        &mut self,
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: &LogicCondition,
        register_number: u64,
    );
    fn if_condition_logic(
        &mut self,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
        register_number: u64,
    );
}
