use crate::codegen::Codegen;
use crate::{ast, semantic};

pub struct Backend;

impl Backend {
    pub const fn new() -> Self {
        Self
    }
}

impl Codegen for Backend {
    type Backend = Self;

    fn function_declaration(&self, _fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend {
        todo!()
    }

    fn constant(&self, _const_decl: &ast::Constant<'_>) -> Self::Backend {
        todo!()
    }

    fn types(&self, _type_decl: &ast::StructTypes<'_>) -> Self::Backend {
        todo!()
    }

    fn function_statement(&mut self, _fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend {
        todo!()
    }

    fn let_binding(&self, _let_decl: &ast::LetBinding<'_>) -> Self::Backend {
        todo!()
    }

    fn call(&self, _call: &ast::FunctionCall<'_>) -> Self::Backend {
        todo!()
    }

    fn expression_value(&self, _expression: &semantic::Value) -> Self::Backend {
        todo!()
    }
    fn expression_const(&self, _expression: &semantic::Constant) -> Self::Backend {
        todo!()
    }
}
