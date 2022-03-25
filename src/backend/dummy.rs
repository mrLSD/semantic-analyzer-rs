use crate::codegen::Codegen;
use crate::semantic::{ExpressionResult, Value};
use crate::{ast, semantic};

pub struct Backend {
    stack: Vec<(String, String)>,
}

impl Backend {
    pub const fn new() -> Self {
        Self { stack: vec![] }
    }

    pub fn set_stack(&mut self, fn_name: &str, stack: String) {
        self.stack.push((fn_name.to_string(), stack));
    }

    pub const fn get_stack(&self) -> &Vec<(String, String)> {
        &self.stack
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

    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult) {
        self.set_stack(
            "let_binding",
            format!("%{} = alloca {}", let_decl.inner_name, let_decl.inner_type),
        );
        let val = match expr_result {
            ExpressionResult::PrimitiveValue(v) => format!("{v:?}"),
            ExpressionResult::Register(v) => format!("%{v:?}"),
        };
        self.set_stack(
            "let_binding",
            format!(
                "store {} {val}, ptr %{}",
                let_decl.inner_type, let_decl.inner_name
            ),
        );
    }

    fn call(
        &self,
        _call: &ast::FunctionCall<'_>,
        _params: Vec<ExpressionResult>,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }

    fn expression_value(&mut self, value: &Value, register_number: u64) {
        self.set_stack(
            "expression_value",
            format!(
                "%{register_number:?} = load {}, ptr {}",
                value.inner_type, value.inner_name
            ),
        );
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
