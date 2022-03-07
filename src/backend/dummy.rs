use crate::codegen::Codegen;
use crate::semantic::ExpressionResult;
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

    fn call(&self, _call: &ast::FunctionCall<'_>, _register_number: u64) -> Self::Backend {
        todo!()
    }

    fn expression_value(
        &self,
        _expression: &semantic::Value,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }
    fn expression_const(
        &self,
        _expression: &semantic::Constant,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }
    fn expression_operation(
        &self,
        _operation: &ast::ExpressionOperations,
        _left_value: &semantic::ExpressionResult,
        _right_value: &semantic::ExpressionResult,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }
    fn expression_function_return(&self, _expr_result: &ExpressionResult) -> Self::Backend {
        todo!()
    }
}
