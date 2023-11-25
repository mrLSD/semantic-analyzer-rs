//! # Semantic types
//! Semantic analyzer result state types.
//! It contains `SemanticStack` as Semantic results
//! Context data.

use super::condition::{Condition, LogicCondition};
use super::expression::{ExpressionOperations, ExpressionResult};
use super::types::StructTypes;
use super::{Constant, Function, FunctionStatement, LabelName, Value};

/// # Semantic stack
/// Semantic stack represent stack of Semantic Context
#[derive(Debug, Default, Clone, PartialEq)]
pub struct SemanticStack(Vec<SemanticStackContext>);

impl SemanticStack {
    /// Init Semantic stack
    pub fn new() -> Self {
        Self::default()
    }

    /// Push Context data to the stack
    pub fn push(&mut self, value: SemanticStackContext) {
        self.0.push(value);
    }

    /// Get all context stack data as array data
    pub fn get(self) -> Vec<SemanticStackContext> {
        self.0
    }

    /// Push Context to the stack as expression value data
    pub fn expression_value(&mut self, expression: Value, register_number: u64) {
        self.push(SemanticStackContext::ExpressionValue {
            expression,
            register_number,
        });
    }

    /// Push Context to the stack as expression const data
    pub fn expression_const(&mut self, expression: Constant, register_number: u64) {
        self.push(SemanticStackContext::ExpressionConst {
            expression,
            register_number,
        });
    }

    /// Push Context to the stack as expression struct value data
    pub fn expression_struct_value(&mut self, expression: Value, index: u32, register_number: u64) {
        self.push(SemanticStackContext::ExpressionStructValue {
            expression,
            index,
            register_number,
        });
    }

    /// Push Context to the stack as expression operation data
    pub fn expression_operation(
        &mut self,
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    ) {
        self.push(SemanticStackContext::ExpressionOperation {
            operation,
            left_value,
            right_value,
            register_number,
        });
    }

    /// Push Context to the stack as function call data
    pub fn call(&mut self, call: Function, params: Vec<ExpressionResult>, register_number: u64) {
        self.push(SemanticStackContext::Call {
            call,
            params,
            register_number,
        });
    }

    /// Push Context to the stack as let-binding data
    pub fn let_binding(&mut self, let_decl: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::LetBinding {
            let_decl,
            expr_result,
        });
    }

    /// Push Context to the stack as binding data
    pub fn binding(&mut self, val: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::Binding { val, expr_result });
    }

    /// Push Context to the stack as function declaration data
    pub fn function_declaration(&mut self, fn_decl: FunctionStatement) {
        self.push(SemanticStackContext::FunctionDeclaration { fn_decl });
    }

    /// Push Context to the stack as constant data
    pub fn constant(&mut self, const_decl: Constant) {
        self.push(SemanticStackContext::Constant { const_decl });
    }

    /// Push Context to the stack as types data
    pub fn types(&mut self, type_decl: StructTypes) {
        self.push(SemanticStackContext::Types { type_decl });
    }

    /// Push Context to the stack as expression function return data
    pub fn expression_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturn { expr_result });
    }

    /// Push Context to the stack as `expression function return with label` data
    pub fn expression_function_return_with_label(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturnWithLabel { expr_result });
    }

    /// Push Context to the stack as `set label` data
    pub fn set_label(&mut self, label: LabelName) {
        self.push(SemanticStackContext::SetLabel { label });
    }

    /// Push Context to the stack as `jump to` data
    pub fn jump_to(&mut self, label: LabelName) {
        self.push(SemanticStackContext::JumpTo { label });
    }

    /// Push Context to the stack as `if condition expression` data
    pub fn if_condition_expression(
        &mut self,
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    ) {
        self.push(SemanticStackContext::IfConditionExpression {
            expr_result,
            label_if_begin,
            label_if_end,
        });
    }

    /// Push Context to the stack as `condition expression` data
    pub fn condition_expression(
        &mut self,
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
    ) {
        self.push(SemanticStackContext::ConditionExpression {
            left_result,
            right_result,
            condition,
        });
    }

    /// Push Context to the stack as `jump function return` data
    pub fn jump_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::JumpFunctionReturn { expr_result });
    }

    /// Push Context to the stack as `logic condition` data
    pub fn logic_condition(&mut self, logic_condition: LogicCondition) {
        self.push(SemanticStackContext::LogicCondition { logic_condition });
    }

    /// Push Context to the stack as `if condition logic` data
    pub fn if_condition_logic(&mut self, label_if_begin: LabelName, label_if_end: LabelName) {
        self.push(SemanticStackContext::IfConditionLogic {
            label_if_begin,
            label_if_end,
        });
    }
}

/// # Semantic stack Context
/// Context data of Semantic results
#[derive(Debug, Clone, PartialEq)]
pub enum SemanticStackContext {
    ExpressionValue {
        expression: Value,
        register_number: u64,
    },
    ExpressionConst {
        expression: Constant,
        register_number: u64,
    },
    ExpressionStructValue {
        expression: Value,
        index: u32,
        register_number: u64,
    },
    ExpressionOperation {
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    },
    Call {
        call: Function,
        params: Vec<ExpressionResult>,
        register_number: u64,
    },
    LetBinding {
        let_decl: Value,
        expr_result: ExpressionResult,
    },
    Binding {
        val: Value,
        expr_result: ExpressionResult,
    },
    FunctionDeclaration {
        fn_decl: FunctionStatement,
    },
    Constant {
        const_decl: Constant,
    },
    Types {
        type_decl: StructTypes,
    },
    ExpressionFunctionReturn {
        expr_result: ExpressionResult,
    },
    ExpressionFunctionReturnWithLabel {
        expr_result: ExpressionResult,
    },
    SetLabel {
        label: LabelName,
    },
    JumpTo {
        label: LabelName,
    },
    IfConditionExpression {
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    },
    ConditionExpression {
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
    },
    JumpFunctionReturn {
        expr_result: ExpressionResult,
    },
    LogicCondition {
        logic_condition: LogicCondition,
    },
    IfConditionLogic {
        label_if_begin: LabelName,
        label_if_end: LabelName,
    },
}
