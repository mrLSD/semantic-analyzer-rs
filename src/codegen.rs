use crate::types::{
    Condition, Constant, ExpressionOperations, ExpressionResult, Function, FunctionStatement,
    LabelName, LogicCondition, StructTypes, Value,
};

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
    fn function_declaration(&mut self, fn_decl: &FunctionStatement) {
        self.push(StackKind::FunctionDeclaration {
            fn_decl: fn_decl.clone(),
        });
    }

    fn constant(&mut self, const_decl: &Constant) {
        self.push(StackKind::Constant {
            const_decl: const_decl.clone(),
        });
    }

    fn types(&mut self, type_decl: &StructTypes) {
        self.push(StackKind::Types {
            type_decl: type_decl.clone(),
        });
    }

    fn function_statement(&mut self, fn_decl: &FunctionStatement) {
        self.push(StackKind::FunctionStatement {
            fn_decl: fn_decl.clone(),
        });
    }

    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult) {
        self.push(StackKind::LetBinding {
            let_decl: let_decl.clone(),
            expr_result: expr_result.clone(),
        });
    }

    fn binding(&mut self, val: &Value, expr_result: &ExpressionResult) {
        self.push(StackKind::Binding {
            val: val.clone(),
            expr_result: expr_result.clone(),
        });
    }

    fn call(&mut self, call: &Function, params: Vec<ExpressionResult>, register_number: u64) {
        self.push(StackKind::Call {
            call: call.clone(),
            params,
            register_number,
        });
    }

    fn expression_value(&mut self, expression: &Value, register_number: u64) {
        self.push(StackKind::ExpressionValue {
            expression: expression.clone(),
            register_number,
        });
    }

    fn expression_struct_value(&mut self, expression: &Value, index: u64, register_number: u64) {
        self.push(StackKind::ExpressionStructValue {
            expression: expression.clone(),
            index,
            register_number,
        });
    }

    fn expression_const(&mut self, expression: &Constant, register_number: u64) {
        self.push(StackKind::ExpressionConst {
            expression: expression.clone(),
            register_number,
        });
    }

    fn expression_operation(
        &mut self,
        operation: &ExpressionOperations,
        left_value: &ExpressionResult,
        right_value: &ExpressionResult,
        register_number: u64,
    ) {
        self.push(StackKind::ExpressionOperation {
            operation: operation.clone(),
            left_value: left_value.clone(),
            right_value: right_value.clone(),
            register_number,
        });
    }

    fn expression_function_return(&mut self, expr_result: &ExpressionResult) {
        self.push(StackKind::ExpressionFunctionReturnWithLabel {
            expr_result: expr_result.clone(),
        });
    }

    fn jump_function_return(&mut self, expr_result: &ExpressionResult) {
        self.push(StackKind::JumpFunctionReturn {
            expr_result: expr_result.clone(),
        });
    }

    fn set_label(&mut self, label: &LabelName) {
        self.push(StackKind::SetLabel {
            label: label.clone(),
        });
    }

    fn expression_function_return_with_label(&mut self, expr_result: &ExpressionResult) {
        self.push(StackKind::ExpressionFunctionReturnWithLabel {
            expr_result: expr_result.clone(),
        });
    }

    fn condition_expression(
        &mut self,
        left_result: &ExpressionResult,
        right_result: &ExpressionResult,
        condition: &Condition,
        register_number: u64,
    ) {
        self.push(StackKind::ConditionExpression {
            left_result: left_result.clone(),
            right_result: right_result.clone(),
            condition: condition.clone(),
            register_number,
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

    fn if_condition_expression(
        &mut self,
        expr_result: &ExpressionResult,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
    ) {
        self.push(StackKind::IfConditionExpression {
            expr_result: expr_result.clone(),
            label_if_begin: label_if_begin.clone(),
            label_if_end: label_if_end.clone(),
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

    fn jump_to(&mut self, label: &LabelName) {
        self.push(StackKind::JumpTo {
            label: label.clone(),
        });
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum StackKind {
    FunctionDeclaration {
        fn_decl: FunctionStatement,
    },
    Constant {
        const_decl: Constant,
    },
    Types {
        type_decl: StructTypes,
    },

    FunctionStatement {
        fn_decl: FunctionStatement,
    },
    LetBinding {
        let_decl: Value,
        expr_result: ExpressionResult,
    },
    Binding {
        val: Value,
        expr_result: ExpressionResult,
    },
    Call {
        call: Function,
        params: Vec<ExpressionResult>,
        register_number: u64,
    },
    ExpressionValue {
        expression: Value,
        register_number: u64,
    },
    ExpressionStructValue {
        expression: Value,
        index: u64,
        register_number: u64,
    },
    ExpressionConst {
        expression: Constant,
        register_number: u64,
    },
    ExpressionOperation {
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    },
    ExpressionFunctionReturn {
        expr_result: ExpressionResult,
    },
    JumpFunctionReturn {
        expr_result: ExpressionResult,
    },
    SetLabel {
        label: LabelName,
    },
    ExpressionFunctionReturnWithLabel {
        expr_result: ExpressionResult,
    },
    ConditionExpression {
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
        register_number: u64,
    },
    LogicCondition {
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: LogicCondition,
        register_number: u64,
    },
    IfConditionExpression {
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    },
    IfConditionLogic {
        label_if_begin: LabelName,
        label_if_end: LabelName,
        register_number: u64,
    },
    JumpTo {
        label: LabelName,
    },
}

pub trait Codegen {
    fn function_declaration(&mut self, fn_decl: &FunctionStatement);
    fn constant(&mut self, const_decl: &Constant);
    fn types(&mut self, type_decl: &StructTypes);
    fn function_statement(&mut self, fn_decl: &FunctionStatement);
    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult);
    fn binding(&mut self, val: &Value, expr_result: &ExpressionResult);
    fn call(&mut self, call: &Function, params: Vec<ExpressionResult>, register_number: u64);
    fn expression_value(&mut self, expression: &Value, register_number: u64);
    fn expression_struct_value(&mut self, expression: &Value, index: u64, register_number: u64);
    fn expression_const(&mut self, expression: &Constant, register_number: u64);
    fn expression_operation(
        &mut self,
        operation: &ExpressionOperations,
        left_value: &ExpressionResult,
        right_value: &ExpressionResult,
        register_number: u64,
    );
    fn expression_function_return(&mut self, expr_result: &ExpressionResult);
    fn jump_function_return(&mut self, expr_result: &ExpressionResult);
    fn set_label(&mut self, label: &LabelName);
    fn expression_function_return_with_label(&mut self, expr_result: &ExpressionResult);
    fn condition_expression(
        &mut self,
        left_result: &ExpressionResult,
        right_result: &ExpressionResult,
        condition: &Condition,
        register_number: u64,
    );
    fn logic_condition(
        &mut self,
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: &LogicCondition,
        register_number: u64,
    );
    fn if_condition_expression(
        &mut self,
        expr_result: &ExpressionResult,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
    );
    fn if_condition_logic(
        &mut self,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
        register_number: u64,
    );
    fn jump_to(&mut self, label: &LabelName);
}
