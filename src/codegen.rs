use crate::semantic::ExpressionResult;
use crate::{ast, semantic};

pub trait Codegen {
    type Backend;

    fn function_declaration(&self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn constant(&self, const_decl: &ast::Constant<'_>) -> Self::Backend;
    fn types(&self, type_decl: &ast::StructTypes<'_>) -> Self::Backend;
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn let_binding(
        &self,
        let_decl: &ast::LetBinding<'_>,
        expr_result: &ExpressionResult,
    ) -> Self::Backend;
    fn call(&self, call: &ast::FunctionCall<'_>, register_number: u64) -> Self::Backend;
    fn expression_value(&self, expression: &semantic::Value, register_number: u64)
        -> Self::Backend;
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
}
