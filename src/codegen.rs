use crate::ast;
use crate::types::{Constant, ExpressionResult, Function, FunctionStatement, LabelName, Value};

#[allow(dead_code, clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq)]
pub struct CodegenStack(Vec<StackKind>);

impl CodegenStack {
    #[allow(dead_code)]
    pub fn push(&mut self, value: StackKind) {
        self.0.push(value);
    }
}

impl Codegen for CodegenStack {
    type Backend = ();

    fn function_declaration(&mut self, fn_decl: &FunctionStatement) {
        self.push(StackKind::FunctionDeclaration {
            fn_decl: fn_decl.clone(),
        });
    }

    fn constant(&self, _const_decl: &ast::Constant<'_>) {
        todo!()
    }

    fn types(&self, _type_decl: &ast::StructTypes<'_>) {
        todo!()
    }

    fn function_statement(&mut self, _fn_decl: &ast::FunctionStatement<'_>) {
        todo!()
    }

    fn let_binding(&mut self, _let_decl: &Value, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn binding(&mut self, _val: &Value, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn call(&self, _call: &Function, _params: Vec<ExpressionResult>, _register_number: u64) {
        todo!()
    }

    fn expression_value(&mut self, _expression: &Value, _register_number: u64) {
        todo!()
    }

    fn expression_struct_value(&mut self, _expression: &Value, _index: u64, _register_number: u64) {
        todo!()
    }

    fn expression_const(&self, _expression: &Constant, _register_number: u64) {
        todo!()
    }

    fn expression_operation(
        &self,
        _operation: &ast::ExpressionOperations,
        _left_value: &ExpressionResult,
        _right_value: &ExpressionResult,
        _register_number: u64,
    ) {
        todo!()
    }

    fn expression_function_return(&self, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn jump_function_return(&self, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn set_label(&self, _label: &LabelName) {
        todo!()
    }

    fn expression_function_return_with_label(&self, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn condition_expression(
        &mut self,
        _left_result: &ExpressionResult,
        _right_result: &ExpressionResult,
        _condition: &ast::Condition,
        _register_number: u64,
    ) {
        todo!()
    }

    fn logic_condition(
        &mut self,
        _left_condition_register: u64,
        _right_condition_register: u64,
        _logic_condition: &ast::LogicCondition,
        _register_number: u64,
    ) {
        todo!()
    }

    fn if_condition_expression(
        &mut self,
        _expr_result: &ExpressionResult,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
    ) {
        todo!()
    }

    fn if_condition_logic(
        &mut self,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
        _register_number: u64,
    ) {
        todo!()
    }

    fn jump_to(&mut self, _label: &LabelName) {
        todo!()
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum StackKind {
    FunctionDeclaration { fn_decl: FunctionStatement },
    /*
    Constant {
        const_decl: ast::Constant<'a>,
    },
    Types {
        type_decl: ast::StructTypes<'a>,
    },
    FunctionStatement {
        fn_decl: ast::FunctionStatement<'a>,
    },
    LetBinding {
        let_decl: Value,
        expr_result: ExpressionResult,
    },
    Binding {
        val: Value,
        expr_result: ExpressionResult,
    },
    Call {
        call: Function,
        params: Vec<ExpressionResult>,
        register_number: u64,
    },
    ExpressionValue {
        expression: Value,
        register_number: u64,
    },
    ExpressionConst {
        expression: Constant,
        register_number: u64,
    },
    ExpressionOperation {
        operation: ast::ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    },
    ExpressionFunctionReturn {
        expr_result: ExpressionResult,
    },
    JumpFunctionReturn {
        expr_result: ExpressionResult,
    },
    SetLabel {
        label: LabelName,
    },
    ExpressionFunctionReturnWithLabel {
        expr_result: ExpressionResult,
    },
    ConditionExpression {
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: ast::Condition,
        register_number: u64,
    },
    LogicCondition {
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: ast::LogicCondition,
        register_number: u64,
    },
    IfConditionExpression {
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    },
    IfConditionLogic {
        label_if_begin: LabelName,
        label_if_end: LabelName,
        register_number: u64,
    },
    JumpTo {
        label: LabelName,
    },
    */
}

pub trait Codegen {
    type Backend;
    fn function_declaration(&mut self, fn_decl: &FunctionStatement);
    fn constant(&self, const_decl: &ast::Constant<'_>);
    fn types(&self, type_decl: &ast::StructTypes<'_>);
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>);
    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult);
    fn binding(&mut self, val: &Value, expr_result: &ExpressionResult);
    fn call(&self, call: &Function, params: Vec<ExpressionResult>, register_number: u64);
    fn expression_value(&mut self, expression: &Value, register_number: u64);
    fn expression_struct_value(&mut self, expression: &Value, index: u64, register_number: u64);
    fn expression_const(&self, expression: &Constant, register_number: u64);
    fn expression_operation(
        &self,
        operation: &ast::ExpressionOperations,
        left_value: &ExpressionResult,
        right_value: &ExpressionResult,
        register_number: u64,
    );
    fn expression_function_return(&self, expr_result: &ExpressionResult);
    fn jump_function_return(&self, expr_result: &ExpressionResult);
    fn set_label(&self, label: &LabelName);
    fn expression_function_return_with_label(&self, expr_result: &ExpressionResult);
    fn condition_expression(
        &mut self,
        left_result: &ExpressionResult,
        right_result: &ExpressionResult,
        condition: &ast::Condition,
        register_number: u64,
    );
    fn logic_condition(
        &mut self,
        left_condition_register: u64,
        right_condition_register: u64,
        logic_condition: &ast::LogicCondition,
        register_number: u64,
    );
    fn if_condition_expression(
        &mut self,
        expr_result: &ExpressionResult,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
    );
    fn if_condition_logic(
        &mut self,
        label_if_begin: &LabelName,
        label_if_end: &LabelName,
        register_number: u64,
    );
    fn jump_to(&mut self, label: &LabelName);
}
