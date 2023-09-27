//! # Semantic types
//! Semantic analyzer result state types.

use super::condition::{Condition, LogicCondition};
use super::expression::{ExpressionOperations, ExpressionResult};
use super::types::StructTypes;
use super::{Constant, Function, FunctionStatement, LabelName, Value};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SemanticStack(Vec<SemanticStackContext>);

impl SemanticStack {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, value: SemanticStackContext) {
        self.0.push(value);
    }

    pub fn get(self) -> Vec<SemanticStackContext> {
        self.0
    }

    pub fn expression_value(&mut self, expression: Value) {
        self.push(SemanticStackContext::ExpressionValue { expression });
    }

    pub fn expression_const(&mut self, expression: Constant) {
        self.push(SemanticStackContext::ExpressionConst { expression });
    }

    pub fn expression_struct_value(&mut self, expression: Value, index: u32) {
        self.push(SemanticStackContext::ExpressionStructValue { expression, index });
    }

    pub fn expression_operation(
        &mut self,
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
    ) {
        self.push(SemanticStackContext::ExpressionOperation {
            operation,
            left_value,
            right_value,
        });
    }

    pub fn call(&mut self, call: Function, params: Vec<ExpressionResult>) {
        self.push(SemanticStackContext::Call { call, params });
    }

    pub fn let_binding(&mut self, let_decl: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::LetBinding {
            let_decl,
            expr_result,
        });
    }

    pub fn binding(&mut self, val: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::Binding { val, expr_result });
    }

    pub fn function_declaration(&mut self, fn_decl: FunctionStatement) {
        self.push(SemanticStackContext::FunctionDeclaration { fn_decl });
    }

    pub fn constant(&mut self, const_decl: Constant) {
        self.push(SemanticStackContext::Constant { const_decl });
    }

    pub fn types(&mut self, type_decl: StructTypes) {
        self.push(SemanticStackContext::Types { type_decl });
    }

    pub fn expression_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturnWithLabel { expr_result });
    }

    pub fn expression_function_return_with_label(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturnWithLabel { expr_result });
    }

    pub fn set_label(&mut self, label: LabelName) {
        self.push(SemanticStackContext::SetLabel { label });
    }

    pub fn jump_to(&mut self, label: LabelName) {
        self.push(SemanticStackContext::JumpTo { label });
    }

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

    pub fn jump_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::JumpFunctionReturn { expr_result });
    }

    pub fn logic_condition(&mut self, logic_condition: LogicCondition) {
        self.push(SemanticStackContext::LogicCondition { logic_condition });
    }

    pub fn if_condition_logic(&mut self, label_if_begin: LabelName, label_if_end: LabelName) {
        self.push(SemanticStackContext::IfConditionLogic {
            label_if_begin,
            label_if_end,
        });
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SemanticStackContext {
    ExpressionValue {
        expression: Value,
    },
    ExpressionConst {
        expression: Constant,
    },
    ExpressionStructValue {
        expression: Value,
        index: u32,
    },
    ExpressionOperation {
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
    },
    Call {
        call: Function,
        params: Vec<ExpressionResult>,
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
