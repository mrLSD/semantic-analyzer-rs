use crate::ast::{Condition, LogicCondition};
use crate::semantic::{ExpressionResult, LabelName, Value};
use crate::{ast, semantic};

pub trait Codegen {
    type Backend;
    fn function_declaration(&self, _fn_decl: &ast::FunctionStatement<'_>);
    fn constant(&self, const_decl: &ast::Constant<'_>);
    fn types(&self, type_decl: &ast::StructTypes<'_>);
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>);
    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult<'_>);
    fn binding(&mut self, val: &Value, expr_result: &ExpressionResult<'_>);
    fn call(
        &self,
        call: &ast::FunctionCall<'_>,
        params: Vec<ExpressionResult<'_>>,
        register_number: u64,
    );
    fn expression_value(&mut self, expression: &Value, register_number: u64);
    fn expression_const(&self, expression: &semantic::Constant, register_number: u64);
    fn expression_operation(
        &self,
        operation: &ast::ExpressionOperations,
        left_value: &ExpressionResult<'_>,
        right_value: &ExpressionResult<'_>,
        register_number: u64,
    );
    fn expression_function_return(&self, expr_result: &ExpressionResult<'_>);
    fn jump_function_return(&self, expr_result: &ExpressionResult<'_>);
    fn set_label(&self, label: &LabelName);
    fn expression_function_return_with_label(&self, expr_result: &ExpressionResult<'_>);
    fn condition_expression(
        &mut self,
        left_result: &ExpressionResult<'_>,
        right_result: &ExpressionResult<'_>,
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
        expr_result: &ExpressionResult<'_>,
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
