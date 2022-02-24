use crate::ast;

pub trait Codegen {
    type Backend;

    fn function_declaration(&self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn constant(&self, const_decl: &ast::Constant<'_>) -> Self::Backend;
    fn types(&self, type_decl: &ast::StructTypes<'_>) -> Self::Backend;
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend;
    fn let_binding(&self, let_decl: &ast::LetBinding<'_>) -> Self::Backend;
    fn call(&self, call: &ast::FunctionCall<'_>) -> Self::Backend;
    fn expression(&self, expression: &ast::Expression) -> &Self;
}
