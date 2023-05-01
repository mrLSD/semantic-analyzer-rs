use crate::ast::{Expression, FunctionStatement};
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

    fn function_statement(&mut self, _fn_decl: &FunctionStatement<'_>) -> Self::Backend {
        todo!()
    }

    fn expression(&self, _expression: &Expression) -> &Self {
        todo!()
    }
}
