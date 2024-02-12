//! # Semantic types
//! Semantic analyzer result state types.
//! It contains `SemanticStack` as Semantic results Context data.

use super::condition::{Condition, LogicCondition};
use super::expression::{ExpressionOperations, ExpressionResult};
use super::types::StructTypes;
use super::{Constant, Function, FunctionParameter, FunctionStatement, LabelName, Value};
use crate::semantic::State;
use crate::types::block_state::BlockState;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::Rc;

/// Semantic Context trait contain instructions set functions
/// for Global Stack context. It includes:
/// - functions
/// - types
/// - constants
pub trait GlobalSemanticContext {
    fn function_declaration(&mut self, fn_decl: FunctionStatement);
    fn constant(&mut self, const_decl: Constant);
    fn types(&mut self, type_decl: StructTypes);
}

/// Semantic Context trait contain instructions set functions
/// for the Stack context.
pub trait SemanticContext {
    fn expression_value(&mut self, expression: Value, register_number: u64);
    fn expression_const(&mut self, expression: Constant, register_number: u64);
    fn expression_struct_value(&mut self, expression: Value, index: u32, register_number: u64);
    fn expression_operation(
        &mut self,
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    );
    fn call(&mut self, call: Function, params: Vec<ExpressionResult>, register_number: u64);
    fn let_binding(&mut self, let_decl: Value, expr_result: ExpressionResult);
    fn binding(&mut self, val: Value, expr_result: ExpressionResult);
    fn expression_function_return(&mut self, expr_result: ExpressionResult);
    fn expression_function_return_with_label(&mut self, expr_result: ExpressionResult);
    fn set_label(&mut self, label: LabelName);
    fn jump_to(&mut self, label: LabelName);
    fn if_condition_expression(
        &mut self,
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    );
    fn condition_expression(
        &mut self,
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
        register_number: u64,
    );
    fn jump_function_return(&mut self, expr_result: ExpressionResult);
    fn logic_condition(
        &mut self,
        logic_condition: LogicCondition,
        left_register_result: u64,
        right_register_result: u64,
        register_number: u64,
    );
    fn if_condition_logic(
        &mut self,
        label_if_begin: LabelName,
        label_if_end: LabelName,
        result_register: u64,
    );
    fn function_arg(&mut self, value: Value, func_arg: FunctionParameter);
}

/// Semantic Context trait contains custom instruction implementation
/// to flexibly extend context instructions.
pub trait SemanticContextInstruction: Debug + Clone {}

/// Extended Expression for semantic analyzer.
pub trait ExtendedExpression: Debug + Clone + PartialEq {
    /// Custom expression. Ast should be received from `GetAst` trait.
    fn expression<I: SemanticContextInstruction>(
        &self,
        state: &mut State<Self, I>,
        block_state: &Rc<RefCell<BlockState<I>>>,
    ) -> ExpressionResult;
}

/// # Semantic stack
/// Semantic stack represent stack of Semantic Context results
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct SemanticStack<I: SemanticContextInstruction>(Vec<SemanticStackContext<I>>);

impl<I: SemanticContextInstruction> SemanticStack<I> {
    /// Init Semantic stack
    #[must_use]
    pub const fn new() -> Self {
        Self(vec![])
    }

    /// Push Context data to the stack
    pub fn push(&mut self, value: SemanticStackContext<I>) {
        self.0.push(value);
    }

    /// Get all context stack data as array data
    #[must_use]
    pub fn get(self) -> Vec<SemanticStackContext<I>> {
        self.0
    }
}

impl<I: SemanticContextInstruction> GlobalSemanticContext for SemanticStack<I> {
    /// Push Context to the stack as function declaration data.
    /// Function declaration instruction.
    ///
    /// ## Parameters
    /// - `fn_decl` - function declaration parameters
    fn function_declaration(&mut self, fn_decl: FunctionStatement) {
        self.push(SemanticStackContext::FunctionDeclaration { fn_decl });
    }

    /// Push Context to the stack as constant data.
    /// Constant declaration instruction.
    ///
    /// ## Parameters
    /// - `const_decl` - constant declaration parameters
    fn constant(&mut self, const_decl: Constant) {
        self.push(SemanticStackContext::Constant { const_decl });
    }

    /// Push Context to the stack as types data.
    /// Types declaration instruction.
    ///
    /// ## Parameters
    /// - `type_decl` - type declaration parameters
    fn types(&mut self, type_decl: StructTypes) {
        self.push(SemanticStackContext::Types { type_decl });
    }
}

impl<I: SemanticContextInstruction> SemanticContext for SemanticStack<I> {
    /// Push Context to the stack as expression value data.
    ///
    /// ## Parameters
    /// - `expression` - contains expression value
    /// - `register_number` - register to store result data
    fn expression_value(&mut self, expression: Value, register_number: u64) {
        self.push(SemanticStackContext::ExpressionValue {
            expression,
            register_number,
        });
    }

    /// Push Context to the stack as expression const data.
    ///
    /// ## Parameters
    /// - `expression` - contains expression constant
    /// - `register_number` - register to store result data
    fn expression_const(&mut self, expression: Constant, register_number: u64) {
        self.push(SemanticStackContext::ExpressionConst {
            expression,
            register_number,
        });
    }

    /// Push Context to the stack as expression struct value data.
    ///
    /// ## Parameters
    /// - `expression` - contains expression value for specific `Structure` attribute
    /// - `index` - represent attribute index in the `Structure` type
    /// - `register_number` - register to store result data
    fn expression_struct_value(&mut self, expression: Value, index: u32, register_number: u64) {
        self.push(SemanticStackContext::ExpressionStructValue {
            expression,
            index,
            register_number,
        });
    }

    /// Push Context to the stack as expression operation data.
    /// `expression_operation` imply operation between `left_value` and
    /// `right_value` and store result to `register_number`.
    ///
    /// ## Parameters
    /// - `operation` - specific operation
    /// - `left_value` - left expression result
    /// - `right_value` - right expression result
    /// - `register_number` - register to store result of expression operation
    fn expression_operation(
        &mut self,
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    ) {
        self.push(SemanticStackContext::ExpressionOperation {
            operation,
            left_value,
            right_value,
            register_number,
        });
    }

    /// Push Context to the stack as function call data.
    /// Function call instruction with parameters and result data.
    ///
    /// ## Parameters
    /// - `call` - function declaration data
    /// - `params` - function parameters
    ///  - `register_number` - register to store result of function call
    fn call(&mut self, call: Function, params: Vec<ExpressionResult>, register_number: u64) {
        self.push(SemanticStackContext::Call {
            call,
            params,
            register_number,
        });
    }

    /// Push Context to the stack as let-binding data.
    /// Let binding instruction that "bind" expression result to
    /// the new value.
    ///
    /// ## Parameters
    /// - `let_decl` - value declaration
    /// -  `expr_result` - expression result that will be bind to the value
    fn let_binding(&mut self, let_decl: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::LetBinding {
            let_decl,
            expr_result,
        });
    }

    /// Push Context to the stack as binding data.
    /// Binding instruction that "bind" expression result to
    /// the old. previously init value.
    ///
    /// ## Parameters
    /// - `val` - value declaration
    /// -  `expr_result` - expression result that will be bind to the value
    fn binding(&mut self, val: Value, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::Binding { val, expr_result });
    }

    /// Push Context to the stack as expression function return data.
    /// Return instruction, should be used in the end of functions.
    /// Alwats should be only once.
    ///
    /// ## Parameters
    /// - `expr_result` - result data for the return
    fn expression_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturn { expr_result });
    }

    /// Push Context to the stack as `expression function return with label` data.
    /// Return instruction with additional logic. Most useful case when
    /// `return` previously was call from `if-body` or `loop-body.`.
    /// As additional behavior this `expression_function_return_with_label` should
    /// set `return` label. It will allow `jump-to-return` case. Also
    /// before `return` label Codegen, for normal instruction flow, must
    /// jump to `return` label anyway.
    ///
    /// ## Parameters
    /// - `expr_result` - result data for the return
    fn expression_function_return_with_label(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::ExpressionFunctionReturnWithLabel { expr_result });
    }

    /// Push Context to the stack as `set label` data.
    /// Set label. Useful for any kind of jump operations and conditional flow.
    ///
    /// ## Parameters
    /// - `label` - label name
    fn set_label(&mut self, label: LabelName) {
        self.push(SemanticStackContext::SetLabel { label });
    }

    /// Push Context to the stack as `jump to` data.
    /// Unconditional direct jump to label.
    ///
    /// ## Parameters
    /// - `label` - label for the jump
    fn jump_to(&mut self, label: LabelName) {
        self.push(SemanticStackContext::JumpTo { label });
    }

    /// Push Context to the stack as `if condition expression` data.
    /// `if-condition expression` represent if-condition, when if expression
    /// is "true" jump to `label_if_begin` else `label_if_end`.
    ///
    /// ## Parameters
    /// - `expr_result` - expression result of `if-condition` for
    /// conditional instruction
    /// - `label_if_begin` - label for jump if expression is "true"
    /// - `label_if_end` - label for jump if expression is "false"
    fn if_condition_expression(
        &mut self,
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    ) {
        self.push(SemanticStackContext::IfConditionExpression {
            expr_result,
            label_if_begin,
            label_if_end,
        });
    }

    /// Push Context to the stack as `condition expression` data.
    /// Condition expression between left and right condition calculation.
    ///
    /// ## Parameters
    /// - `left_result` - left expression result
    /// - `right_result` - right expression result
    /// - `condition` - condition operation
    /// - `register_number` - register to store result of expression operation
    fn condition_expression(
        &mut self,
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
        register_number: u64,
    ) {
        self.push(SemanticStackContext::ConditionExpression {
            left_result,
            right_result,
            condition,
            register_number,
        });
    }

    /// Push Context to the stack as `jump function return` data.
    /// Jump to function return with expression result data. Label for jumping
    /// to return position (always end of function) should be always the same
    /// and should be managed by Codegen.
    ///
    /// ## Parameters
    /// - `expr_result` - expression result for return condition
    fn jump_function_return(&mut self, expr_result: ExpressionResult) {
        self.push(SemanticStackContext::JumpFunctionReturn { expr_result });
    }

    /// Push Context to the stack as `logic condition` data.
    /// Operate with registers: left and right for specific logic condition.
    /// Result of calculation stored to `register_number`.
    ///
    /// ## Parameters
    /// - `left_register_result` - result of left condition
    /// - `right_register_result` - result of right condition
    /// - `register_number` - register to store instruction result
    fn logic_condition(
        &mut self,
        logic_condition: LogicCondition,
        left_register_result: u64,
        right_register_result: u64,
        register_number: u64,
    ) {
        self.push(SemanticStackContext::LogicCondition {
            logic_condition,
            left_register_result,
            right_register_result,
            register_number,
        });
    }

    /// Push Context to the stack as `if condition logic` data.
    /// `if_condition_logic` instruction read data from `result_register`
    /// and conditionally jump: if "true' to `label_if_begin` or
    /// `label_if_end` if "false" (data contained as result after
    /// reading `result_register`).
    ///
    /// ## Parameters
    /// - `label_if_begin` - label for a jump if `result_register` contains
    /// result with "true"
    /// - `label_if_end` - label for a jump if `result_register` contains
    ///  result with "false". It can be not only `if_end` but any kind (for
    /// example `if_else`)
    /// - `result_register` - contains register of previous condition logic
    /// calculations.
    fn if_condition_logic(
        &mut self,
        label_if_begin: LabelName,
        label_if_end: LabelName,
        result_register: u64,
    ) {
        self.push(SemanticStackContext::IfConditionLogic {
            label_if_begin,
            label_if_end,
            result_register,
        });
    }

    /// Push Context to the stack as `function argument` data.
    /// This instruction should allocate pointer (if argument type is
    /// not Ptr) and store argument value to the pointer.
    ///
    /// ## Parameters
    /// - `func_arg` - function parameter data
    fn function_arg(&mut self, value: Value, func_arg: FunctionParameter) {
        self.push(SemanticStackContext::FunctionArg { value, func_arg });
    }
}

/// # Semantic stack Context
/// Context data of Semantic results. Contains type declarations
/// for specific instructions.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(
    feature = "codec",
    derive(Serialize, Deserialize),
    serde(tag = "type", content = "content")
)]
pub enum SemanticStackContext<I: SemanticContextInstruction> {
    ExpressionValue {
        expression: Value,
        register_number: u64,
    },
    ExpressionConst {
        expression: Constant,
        register_number: u64,
    },
    ExpressionStructValue {
        expression: Value,
        index: u32,
        register_number: u64,
    },
    ExpressionOperation {
        operation: ExpressionOperations,
        left_value: ExpressionResult,
        right_value: ExpressionResult,
        register_number: u64,
    },
    Call {
        call: Function,
        params: Vec<ExpressionResult>,
        register_number: u64,
    },
    LetBinding {
        let_decl: Value,
        expr_result: ExpressionResult,
    },
    Binding {
        val: Value,
        expr_result: ExpressionResult,
    },
    FunctionDeclaration {
        fn_decl: FunctionStatement,
    },
    Constant {
        const_decl: Constant,
    },
    Types {
        type_decl: StructTypes,
    },
    ExpressionFunctionReturn {
        expr_result: ExpressionResult,
    },
    ExpressionFunctionReturnWithLabel {
        expr_result: ExpressionResult,
    },
    SetLabel {
        label: LabelName,
    },
    JumpTo {
        label: LabelName,
    },
    IfConditionExpression {
        expr_result: ExpressionResult,
        label_if_begin: LabelName,
        label_if_end: LabelName,
    },
    ConditionExpression {
        left_result: ExpressionResult,
        right_result: ExpressionResult,
        condition: Condition,
        register_number: u64,
    },
    JumpFunctionReturn {
        expr_result: ExpressionResult,
    },
    LogicCondition {
        logic_condition: LogicCondition,
        left_register_result: u64,
        right_register_result: u64,
        register_number: u64,
    },
    IfConditionLogic {
        label_if_begin: LabelName,
        label_if_end: LabelName,
        result_register: u64,
    },
    FunctionArg {
        value: Value,
        func_arg: FunctionParameter,
    },
    ExtendedExpression(Box<I>),
    Test(Box<dyn SemanticContextInstruction>),
}
