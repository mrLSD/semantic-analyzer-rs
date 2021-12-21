use crate::ast;

pub trait Codegen {
    type Backend;

    fn function_declaration(&self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn expression(&self, expression: &ast::Expression) -> &Self;
}
