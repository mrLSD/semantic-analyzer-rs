use crate::ast;

pub trait Codegen {
    fn expression(expression: ast::Expression) -> Self;
}
