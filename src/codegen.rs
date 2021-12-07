use crate::ast;

pub trait Codegen {
    fn function_declaration(&self, fn_decl: ast::FunctionStatement) -> &Self;
    fn expression(&self, expression: ast::Expression) -> &Self;
}
