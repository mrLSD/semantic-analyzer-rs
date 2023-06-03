use crate::ast::Condition;
use crate::semantic::{ExpressionResult, Value};
use crate::{ast, semantic};

pub trait Codegen {
    type Backend;
    fn function_declaration(&self, _fn_decl: &ast::FunctionStatement<'_>);
    fn constant(&self, const_decl: &ast::Constant<'_>);
    fn types(&self, type_decl: &ast::StructTypes<'_>);
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult);
    fn call(
        &self,
        call: &ast::FunctionCall<'_>,
        params: Vec<ExpressionResult>,
        register_number: u64,
    ) -> Self::Backend;
    fn expression_value(&mut self, expression: &Value, register_number: u64);
    fn expression_const(
        &self,
        expression: &semantic::Constant,
        register_number: u64,
    ) -> Self::Backend;
    fn expression_operation(
        &self,
        operation: &ast::ExpressionOperations,
        left_value: &semantic::ExpressionResult,
        right_value: &semantic::ExpressionResult,
        register_number: u64,
    ) -> Self::Backend;
    fn expression_function_return(&self, expr_result: &ExpressionResult) -> Self::Backend;
    fn condition_expression(
        &mut self,
        left_result: &ExpressionResult,
        right_result: &ExpressionResult,
        condition: &Condition,
    );
    fn if_condition_expression(
        &mut self,
        expr_result: &ExpressionResult,
        label_if_begin: &str,
        label_if_end: &str,
    );
    fn if_condition_logic(
        &mut self,
        label_if_begin: &str,
        label_if_end: &str,
        register_number: u64,
    );
    fn if_end(&mut self, label_if_end: &str);
}
