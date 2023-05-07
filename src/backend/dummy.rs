use crate::ast::{Constant, Expression, FunctionStatement, StructTypes};
use crate::codegen::Codegen;

pub struct Backend;

impl Backend {
    pub const fn new() -> Self {
        Self
    }
}

impl Codegen for Backend {
    type Backend = Self;

    fn function_declaration(&self, _fn_decl: &FunctionStatement<'_>) -> Self::Backend {
        todo!()
    }

    fn constant(&self, _const_decl: &Constant<'_>) -> Self::Backend {
        todo!()
    }

    fn types(&self, _type_decl: &StructTypes<'_>) -> Self::Backend {
        todo!()
    }

    fn function_statement(&mut self, _fn_decl: &FunctionStatement<'_>) -> Self::Backend {
        todo!()
    }

    fn expression(&self, _expression: &Expression) -> &Self {
        todo!()
    }
}
