use crate::semantic::{ExpressionResult, Function, LabelName, Value};
use crate::{ast, semantic};

#[allow(dead_code, clippy::module_name_repetitions)]
#[derive(Debug, Clone, PartialEq)]
pub struct CodegenStack<'a>(Vec<StackKind<'a>>);

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq)]
pub enum StackKind<'a> {
    FunctionDeclaration {
        fn_decl: ast::FunctionStatement<'a>,
    },
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
        expression: semantic::Constant,
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
}

pub trait Codegen {
    type Backend;
    fn function_declaration(&self, _fn_decl: &ast::FunctionStatement<'_>);
    fn constant(&self, const_decl: &ast::Constant<'_>);
    fn types(&self, type_decl: &ast::StructTypes<'_>);
    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>);
    fn let_binding(&mut self, let_decl: &Value, expr_result: &ExpressionResult);
    fn binding(&mut self, val: &Value, expr_result: &ExpressionResult);
    fn call(&self, call: &Function, params: Vec<ExpressionResult>, register_number: u64);
    fn expression_value(&mut self, expression: &Value, register_number: u64);
    fn expression_const(&self, expression: &semantic::Constant, register_number: u64);
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
