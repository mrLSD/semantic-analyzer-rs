use crate::codegen::Codegen;
use crate::types::{
    Condition, Constant, ExpressionOperations, ExpressionResult, ExpressionResultValue, Function,
    FunctionStatement, LabelName, LogicCondition, StructTypes, Value,
};

pub struct Backend {
    stack: Vec<(String, String)>,
}

impl Backend {
    pub const fn new() -> Self {
        Self { stack: vec![] }
    }

    pub fn set_stack(&mut self, fn_name: &str, stack: String) {
        self.stack.push((fn_name.to_string(), stack));
    }

    pub const fn get_stack(&self) -> &Vec<(String, String)> {
        &self.stack
    }
}

impl Codegen for Backend {
    type Backend = Self;

    fn function_declaration(&mut self, _fn_decl: &FunctionStatement) {
        todo!()
    }

    fn constant(&mut self, _const_decl: &Constant) {
        todo!()
    }

    fn types(&mut self, _type_decl: &StructTypes) {
        todo!()
    }

    fn function_statement(&mut self, _fn_decl: &FunctionStatement) {
        todo!()
    }

    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult) {
        self.set_stack(
            "let_binding",
            format!(
                "%{} = alloca {}",
                let_decl.inner_name.to_string(),
                let_decl.inner_type.to_string()
            ),
        );
        let val = match expr_result.clone().expr_value {
            ExpressionResultValue::PrimitiveValue(v) => format!("{v:?}"),
            ExpressionResultValue::Register(v) => format!("%{v:?}"),
        };
        self.set_stack(
            "let_binding",
            format!(
                "store {} {val}, ptr %{}",
                let_decl.inner_type.to_string(),
                let_decl.inner_name.to_string()
            ),
        );
    }
    fn binding(&mut self, _val: &Value, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn call(&mut self, _call: &Function, _params: Vec<ExpressionResult>, _register_number: u64) {
        todo!()
    }

    fn expression_value(&mut self, value: &Value, register_number: u64) {
        self.set_stack(
            "expression_value",
            format!(
                "%{register_number:?} = load {}, ptr %{}",
                value.inner_type.to_string(),
                value.inner_name.to_string()
            ),
        );
    }
    fn expression_struct_value(&mut self, _expression: &Value, _index: u64, _register_number: u64) {
        todo!()
    }
    fn expression_const(&mut self, _expression: &Constant, _register_number: u64) {
        todo!()
    }
    fn expression_operation(
        &mut self,
        _operation: &ExpressionOperations,
        _left_value: &ExpressionResult,
        _right_value: &ExpressionResult,
        _register_number: u64,
    ) {
        todo!()
    }
    fn expression_function_return_with_label(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn jump_function_return(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn expression_function_return(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn condition_expression(
        &mut self,
        _left_result: &ExpressionResult,
        _right_result: &ExpressionResult,
        _condition: &Condition,
        _register_number: u64,
    ) {
        todo!();
    }

    fn logic_condition(
        &mut self,
        _left_condition_register: u64,
        _right_condition_register: u64,
        _logic_condition: &LogicCondition,
        _register_number: u64,
    ) {
        todo!()
    }

    fn if_condition_expression(
        &mut self,
        _expr_result: &ExpressionResult,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
    ) {
        todo!();
    }
    fn if_condition_logic(
        &mut self,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
        _register_number: u64,
    ) {
        todo!();
    }
    fn jump_to(&mut self, _label: &LabelName) {
        todo!();
    }
    fn set_label(&mut self, _label: &LabelName) {
        todo!()
    }
}
