//! # Semantic analyzer
//! Semantic analyzer provides algorithms to analyze AST for different
//! rules and generate `Semantic State stack` stack results. AST represent tree
//! nodes of language constructions and fully cover all flow of the program
//! represented through AST. And it's **Turing-complete**.
//!
//! ## Semantic State
//! Semantic State contains basic entities:
//! - `Global State` - global state of semantic analyzer results.
//! - `Context` - stack for `Block state` of each functions body state.
//! - `Errors` - semantic analyzes errors.z

use crate::ast::{self, CodeLocation, GetLocation, GetName, MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS};
use crate::types::block_state::BlockState;
use crate::types::expression::{
    Expression, ExpressionResult, ExpressionResultValue, ExpressionStructValue,
};
use crate::types::semantic::{GlobalSemanticContext, SemanticContext, SemanticStack};
use crate::types::types::{Type, TypeName};
use crate::types::{
    error, Binding, Constant, ConstantName, Function, FunctionCall, FunctionName,
    FunctionParameter, FunctionStatement, InnerValueName, LabelName, LetBinding, Value,
};
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// # Global State
/// Global state can contains state declarations of:
/// - Constants
/// - Types
/// - Functions
/// And Semantic State context results for Global State context:
/// - Context
/// The visibility of Global state limited by current module.
/// `Context` contains results of `Semantic` stack, as result of
/// Semantic analyzer for Global State context. It's can be used for
/// post-verification process, linting, Codegen.
#[derive(Debug)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct GlobalState {
    /// Constants declarations
    pub constants: HashMap<ConstantName, Constant>,
    /// Types declarations
    pub types: HashMap<TypeName, Type>,
    /// Functions declarations
    pub functions: HashMap<FunctionName, Function>,
    /// Context as Semantic Stack Context results contains basic semantic
    /// result tree for Global context state.
    pub context: SemanticStack,
}

/// # State
/// Basic entity that contains:
/// - `Global State` - types, constants, functions declaration and
///   most important - context results of Semantic State stack, that can be
///   used for post-verification and/or Codegen.
/// - `Context` stack for `Block state` of each functions body state
/// - `Error State` contains errors stack as result of Semantic analyzer
#[derive(Debug)]
#[cfg_attr(feature = "codec", derive(Serialize))]
pub struct State {
    /// Global State for current State
    pub global: GlobalState,
    /// Context for all `Block State` stack that related to concrete functions body.
    #[cfg_attr(feature = "codec", serde(skip))]
    pub context: Vec<Rc<RefCell<BlockState>>>,
    /// Error state results stack
    pub errors: Vec<error::StateErrorResult>,
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    /// Init new `State`
    #[must_use]
    pub fn new() -> Self {
        Self {
            global: GlobalState {
                functions: HashMap::new(),
                types: HashMap::new(),
                constants: HashMap::new(),
                context: SemanticStack::new(),
            },
            context: Vec::new(),
            errors: Vec::new(),
        }
    }

    /// Add error to Semantic `Errors State`
    fn add_error(&mut self, err: error::StateErrorResult) {
        self.errors.push(err);
    }

    /// Add `State context` with body state context block
    fn add_state_context(&mut self, state_body: Rc<RefCell<BlockState>>) {
        self.context.push(state_body);
    }

    /// Check is value type exists in `Global State`.
    /// `Primitive` type always return true. For other cases if type doesn't
    /// exist in `Global State`, add errors to `Error State` and return `false` result.
    fn check_type_exists(
        &mut self,
        type_name: &Type,
        val_name: &impl ToString,
        location: &impl GetLocation,
    ) -> bool {
        if let Type::Primitive(_) = type_name {
            return true;
        }
        if !self.global.types.contains_key(&type_name.name()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::TypeNotFound,
                val_name.to_string(),
                location.location(),
            ));
            return false;
        }
        true
    }

    /// Run semantic analyzer that covers all flow for AST.
    /// It's do not return any results, but fill results fir the `Semantic State`.
    ///
    pub fn run(&mut self, data: &ast::Main<'_>) {
        // Execute each kind of analyzing and return errors data.
        // For functions - fetch only declaration for fast-forward
        // identification for using it in functions body.

        // First pass is Imports and Types
        for main in data {
            match main {
                ast::MainStatement::Import(import) => self.import(import),
                ast::MainStatement::Types(types) => self.types(types),
                _ => (),
            }
        }
        // Declaration pass for Constants and Functions
        for main in data {
            match main {
                ast::MainStatement::Constant(constant) => self.constant(constant),
                ast::MainStatement::Function(function) => self.function_declaration(function),
                _ => (),
            }
        }

        // After getting all functions declarations, fetch only functions body
        for main in data {
            if let ast::MainStatement::Function(function) = main {
                self.function_body(function);
            }
        }
    }

    /// Import analyzer (TBD)
    #[allow(clippy::unused_self, clippy::unnecessary_wraps)]
    pub fn import(&self, data: &ast::ImportPath<'_>) {
        if !data.is_empty() {
            let _name = data[0].name();
        }
    }

    /// Types declaration analyzer. Add types to `Global State`.
    /// Currently only one type kind: Structs. And types can't be part of
    /// the `Block State`.
    pub fn types(&mut self, data: &ast::StructTypes<'_>) {
        if self.global.types.contains_key(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::TypeAlreadyExist,
                data.name(),
                data.location(),
            ));
            return;
        }
        let struct_type = Type::Struct(data.clone().into());
        self.global.types.insert(struct_type.name(), struct_type);
        self.global.context.types(data.clone().into());
    }

    /// Check constant value expression.
    /// If expression contains `Constant` check is constant exists.
    /// Values doesn't check as it's just `Primitive Values`.
    /// Also check all expression tree branches.
    /// If `ConstantValue` doesn't exist add error to `Error State` and `return` false result.
    pub fn check_constant_value_expression(
        &mut self,
        data: &Option<(ast::ExpressionOperations, Box<ast::ConstantExpression<'_>>)>,
    ) -> bool {
        // For constant expression skip ExpressionOperations
        if let Some((_, child_data)) = data {
            // Check only Constant value
            match child_data.value.clone() {
                // Check is ConstantValue already exist in global state
                ast::ConstantValue::Constant(const_name) => {
                    if !self
                        .global
                        .constants
                        .contains_key(&const_name.clone().into())
                    {
                        self.add_error(error::StateErrorResult::new(
                            error::StateErrorKind::ConstantNotFound,
                            const_name.name(),
                            const_name.location(),
                        ));
                        return false;
                    }
                    self.check_constant_value_expression(&child_data.operation)
                }
                ast::ConstantValue::Value(_) => true,
            }
        } else {
            true
        }
    }

    /// Constant analyzer. Add it to `Global State`, because constants
    /// can be only global for `Semantic state`, not for `Block state`.
    pub fn constant(&mut self, data: &ast::Constant<'_>) {
        if self.global.constants.contains_key(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ConstantAlreadyExist,
                data.name(),
                data.location(),
            ));
            return;
        }
        if !self.check_constant_value_expression(&data.constant_value.operation) {
            return;
        }
        let const_val: Constant = data.clone().into();
        if !self.check_type_exists(&const_val.constant_type, &const_val.name, data) {
            return;
        }
        self.global
            .constants
            .insert(const_val.name.clone(), const_val.clone());
        self.global.context.constant(const_val);
    }

    /// Function declaration analyze. Add it to Global State/M
    pub fn function_declaration(&mut self, data: &ast::FunctionStatement<'_>) {
        if self.global.functions.contains_key(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::FunctionAlreadyExist,
                data.name(),
                data.location(),
            ));
            return;
        }
        let func_decl: FunctionStatement = data.clone().into();
        let mut force_quite =
            !self.check_type_exists(&func_decl.result_type, &func_decl.name, data);

        // Fetch parameters and check types
        let parameters = func_decl
            .parameters
            .iter()
            .map(|p| {
                force_quite = force_quite || !self.check_type_exists(&p.parameter_type, p, data);
                p.parameter_type.clone()
            })
            .collect();
        // Force quite if errors
        if force_quite {
            return;
        }
        self.global.functions.insert(
            data.name().into(),
            Function {
                inner_name: func_decl.name,
                inner_type: func_decl.result_type,
                parameters,
            },
        );
        self.global
            .context
            .function_declaration(data.clone().into());
    }

    /// Init function parameters.
    /// It's init function parameters as values, same as let-binding.
    /// And add instructions to `SemanticStack`.
    fn init_func_params(
        &mut self,
        function_state: &Rc<RefCell<BlockState>>,
        fn_params: &Vec<ast::FunctionParameter<'_>>,
    ) {
        for fn_param in fn_params {
            let func_param: FunctionParameter = fn_param.clone().into();
            let arg_name = func_param.clone().to_string();

            // Find value in current state and parent states
            let value = function_state
                .borrow()
                .get_value_name(&arg_name.clone().into());
            // Calculate `inner_name` as unique for current and all parent states
            let inner_name: InnerValueName = if value.is_none() {
                // if value not found in all states check and set
                // `inner_value` from value name
                // NOTE: value number not incremented
                arg_name.clone().into()
            } else {
                // Function parameter name can't be with the same name.
                // Produce error
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::FunctionArgumentNameDuplicated,
                    arg_name,
                    CodeLocation::new(1, 1),
                ));
                return;
            };
            // Set value parameters
            let value = Value {
                inner_name: inner_name.clone(),
                inner_type: func_param.parameter_type.clone(),
                mutable: false,
                alloca: false,
                malloc: false,
            };
            // Value inserted only to current state by Value name and Value data
            function_state
                .borrow_mut()
                .values
                .insert(arg_name.into(), value.clone());
            // Set `inner_name` to current state and all parent states
            function_state
                .borrow_mut()
                .set_inner_value_name(&inner_name);

            function_state.borrow_mut().function_arg(value, func_param);
        }
    }

    /// Function body analyze.
    /// It is basic execution entity for program flow.
    /// It's operate sub analyze for function elements. It's contain
    /// Body State for current and child states.
    pub fn function_body(&mut self, data: &ast::FunctionStatement<'_>) {
        // Init empty function body state
        let body_state = Rc::new(RefCell::new(BlockState::new(None)));
        self.add_state_context(body_state.clone());
        // Init function parameters - add to SemanticStackContext
        self.init_func_params(&body_state, &data.parameters);
        // Flag to indicate is function return called
        let mut return_is_called = false;
        // Fetch function elements and gather errors
        for body in &data.body {
            if return_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterReturnDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            match body {
                ast::BodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &body_state);
                }
                ast::BodyStatement::Binding(bind) => {
                    self.binding(bind, &body_state);
                }
                ast::BodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, &body_state);
                }
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state, &None, None);
                }
                ast::BodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &body_state);
                }
                ast::BodyStatement::Expression(expression)
                | ast::BodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &body_state);
                    let expr: Expression = expression.clone().into();
                    // Check is return statement previously called
                    if return_is_called {
                        self.add_error(error::StateErrorResult::new(
                            error::StateErrorKind::ReturnAlreadyCalled,
                            expr.to_string(),
                            expression.location(),
                        ));
                    }
                    if let Some(res) = expr_result {
                        // Check expression type and do not exist from flow
                        self.check_type_exists(&res.expr_type, &expr, &expression.clone());
                        let fn_ty: Type = data.result_type.clone().into();
                        if fn_ty != res.expr_type {
                            self.add_error(error::StateErrorResult::new(
                                error::StateErrorKind::WrongReturnType,
                                expr.to_string(),
                                expression.location(),
                            ));
                        }

                        return_is_called = true;
                        // Check is state contain flag of manual
                        // return from other states, for example:
                        // if-flow, loop-flow
                        if body_state.borrow().manual_return {
                            // First we put expression return calculation for case when
                            // before in the state was return statement. So construct
                            // return expression and jump to return label, set return
                            // label and invoke after that read `return` value from all
                            // previous returns and invoke return instruction itself.
                            body_state
                                .borrow_mut()
                                .expression_function_return_with_label(res);
                        } else {
                            body_state.borrow_mut().expression_function_return(res);
                        }
                    }
                }
            }
        }
        // Check is function contain return
        if !return_is_called {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ReturnNotFound,
                String::new(),
                data.location(),
            ));
        }
    }

    /// # Let-binding statement
    /// Analyze let-binding statement:
    /// 1. Let value bind from expression. First should be analysed
    ///    `expression` for binding value.
    /// 2. Generate value for current state. Special field `inner_name`
    ///    that used as name for `Codegen` should be unique in current
    ///    state and for all parent states. For that `inner_name` the
    ///    inner value name counter incremented.
    /// 3. Set `Value` parameters: `inner_name`,  type and allocation status
    /// 4. Insert value to current values state map: value `name` -> `Data`
    /// 5. Store `inner_name` in current and parent states
    /// 6. Codegen
    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'_>,
        function_state: &Rc<RefCell<BlockState>>,
    ) {
        // Call value analytics before putting let-value to state
        let Some(expr_result) = self.expression(&data.value, function_state) else {
            return;
        };
        let let_data: LetBinding = data.clone().into();

        if let Some(ty) = &let_data.value_type {
            if &expr_result.expr_type != ty {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::WrongLetType,
                    let_data.to_string(),
                    data.location(),
                ));
                return;
            }
        }
        let let_ty = expr_result.expr_type.clone();

        // Find value in current state and parent states
        let value = function_state.borrow().get_value_name(&let_data.name);
        // Calculate `inner_name` as unique for current and all parent states
        let inner_name = value.map_or_else(
            || {
                // if value not found in all states check and set
                // `inner_value` from value name
                function_state
                    .borrow()
                    .get_next_inner_name(&let_data.name.clone().into())
            },
            |val| {
                // Increment inner value name counter for shadowed variable
                // and check variable inner_name for and inner_values in current state
                function_state.borrow().get_next_inner_name(&val.inner_name)
            },
        );
        // Set value parameters
        let value = Value {
            inner_name: inner_name.clone(),
            inner_type: let_ty,
            mutable: let_data.mutable,
            alloca: false,
            malloc: false,
        };
        // Value inserted only to current state by Value name and Value data
        function_state
            .borrow_mut()
            .values
            .insert(let_data.name, value.clone());
        // Set `inner_name` to current state and all parent states
        function_state
            .borrow_mut()
            .set_inner_value_name(&inner_name);

        function_state.borrow_mut().let_binding(value, expr_result);
    }

    /// # Binding statement
    /// Analyze binding statement for mutable variables:
    /// 1. Bind from expression. First should be analysed
    ///    `expression` for binding value.
    /// 2. Read value for current state.
    /// 3. Update value to current values state map: value `name` -> `Data`
    /// 4. Codegen with Store action
    pub fn binding(&mut self, data: &ast::Binding<'_>, function_state: &Rc<RefCell<BlockState>>) {
        // Call value analytics before putting let-value to state
        let Some(expr_result) = self.expression(&data.value, function_state) else {
            return;
        };
        let bind_data: Binding = data.clone().into();

        // Find value in current state and parent states
        let Some(value) = function_state.borrow().get_value_name(&bind_data.name) else {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ValueNotFound,
                bind_data.to_string(),
                data.location(),
            ));
            return;
        };
        // Check is value mutable
        if !value.mutable {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ValueIsNotMutable,
                bind_data.to_string(),
                data.location(),
            ));
            return;
        }
        function_state.borrow_mut().binding(value, expr_result);
    }

    /// # Function-call
    /// Call function with function parameters arguments. Arguments is
    /// expressions.
    /// 1. Check is current function name exists in global state of functions
    /// name.
    /// 2. Analyse expressions for function parameters
    /// 3. Inc register
    /// 4. Generate codegen
    /// Codegen store always result to register even for void result.
    ///
    /// ## Errors
    /// Return error if function name doesn't exist in global state
    pub fn function_call(
        &mut self,
        data: &ast::FunctionCall<'_>,
        body_state: &Rc<RefCell<BlockState>>,
    ) -> Option<Type> {
        let func_call_data: FunctionCall = data.clone().into();
        // Check is function exists in global functions stat
        let Some(func_data) = self.global.functions.get(&func_call_data.name).cloned() else {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::FunctionNotFound,
                func_call_data.to_string(),
                data.location(),
            ));
            return None;
        };
        let fn_type = func_data.inner_type.clone();

        // Analyse function parameters expressions, check their types
        // and set result to array
        let mut params: Vec<ExpressionResult> = vec![];
        for (i, expr) in data.parameters.iter().enumerate() {
            // Types checked in expression, so we don't need additional check
            let expr_result = self.expression(expr, body_state)?;
            if expr_result.expr_type != func_data.parameters[i] {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::FunctionParameterTypeWrong,
                    expr_result.expr_type.to_string(),
                    data.location(),
                ));
                continue;
            }
            params.push(expr_result);
        }

        // Result of function call is stored to register
        body_state.borrow_mut().inc_register();
        let last_register_number = body_state.borrow().last_register_number;
        // Store always result to register even for void result
        body_state
            .borrow_mut()
            .call(func_data, params, last_register_number);
        Some(fn_type)
    }

    /// # condition-expression
    /// Analyse condition operations.
    /// ## Return
    /// Return result register of `condition-expression` calculation.
    pub fn condition_expression(
        &mut self,
        data: &ast::ExpressionLogicCondition<'_>,
        function_body_state: &Rc<RefCell<BlockState>>,
    ) -> u64 {
        // Analyse left expression of left condition
        let left_expr = &data.left.left;
        let left_res = self.expression(left_expr, function_body_state);

        // Analyse right expression of left condition
        let right_expr = &data.left.right;
        let right_res = self.expression(right_expr, function_body_state);

        // If some of the `left` or `right` expression is empty just return with error in the state
        let (Some(left_res), Some(right_res)) = (left_res.clone(), right_res.clone()) else {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ConditionIsEmpty,
                format!("left={left_res:?}, right={right_res:?}"),
                data.left.left.location(),
            ));
            return function_body_state.borrow().last_register_number;
        };

        // Currently strict type comparison
        if left_res.expr_type != right_res.expr_type {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ConditionExpressionWrongType,
                left_res.expr_type.to_string(),
                data.left.left.location(),
            ));
            return function_body_state.borrow().last_register_number;
        }
        if let Type::Primitive(_) = left_res.expr_type {
        } else {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ConditionExpressionNotSupported,
                left_res.expr_type.to_string(),
                data.left.left.location(),
            ));
            return function_body_state.borrow().last_register_number;
        }

        // Increment register
        function_body_state.borrow_mut().inc_register();

        let register_number = function_body_state.borrow_mut().last_register_number;
        // Codegen for left condition and set result to register
        function_body_state.borrow_mut().condition_expression(
            left_res,
            right_res,
            data.left.condition.clone().into(),
            register_number,
        );

        // Analyze right condition
        if let Some(right) = &data.right {
            let left_register_result = function_body_state.borrow_mut().last_register_number;
            // Analyse recursively right part of condition
            let right_register_result = self.condition_expression(&right.1, function_body_state);

            // Increment register
            function_body_state.borrow_mut().inc_register();

            let register_number = function_body_state.borrow_mut().last_register_number;
            // Stategen for logical condition for: left [LOGIC-OP] right
            // The result generated from registers, and stored to
            // new register
            function_body_state.borrow_mut().logic_condition(
                right.0.clone().into(),
                left_register_result,
                right_register_result,
                register_number,
            );
        }
        function_body_state.borrow_mut().last_register_number
    }

    /// # If-condition body
    /// Analyze body for ant if condition:
    /// - if, else, if-else
    /// NOTE: `label_end` - is always already exists
    /// ## Return
    /// Return body statement "return" status
    pub fn if_condition_body(
        &mut self,
        body: &[ast::IfBodyStatement<'_>],
        if_body_state: &Rc<RefCell<BlockState>>,
        label_end: &LabelName,
        label_loop: Option<(&LabelName, &LabelName)>,
    ) -> bool {
        let mut return_is_called = false;
        for body in body {
            if return_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterReturnDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            match body {
                ast::IfBodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, if_body_state);
                }
                ast::IfBodyStatement::Binding(bind) => {
                    self.binding(bind, if_body_state);
                }
                ast::IfBodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, if_body_state);
                }
                ast::IfBodyStatement::If(if_condition) => {
                    self.if_condition(
                        if_condition,
                        if_body_state,
                        &Some(label_end.clone()),
                        label_loop,
                    );
                }
                ast::IfBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, if_body_state);
                }
                ast::IfBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, if_body_state);
                    if let Some(res) = expr_result {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        if_body_state.borrow_mut().jump_function_return(res);
                        if_body_state.borrow_mut().set_return();
                        return_is_called = true;
                    };
                }
            }
        }
        return_is_called
    }

    /// # If-condition loop body
    /// Analyze body for ant if condition:
    /// - if, else, if-else
    /// ## Return
    /// Return body statement "return" status
    pub fn if_condition_loop_body(
        &mut self,
        body: &[ast::IfLoopBodyStatement<'_>],
        if_body_state: &Rc<RefCell<BlockState>>,
        label_if_end: &LabelName,
        label_loop_start: &LabelName,
        label_loop_end: &LabelName,
    ) -> bool {
        let mut return_is_called = false;
        let mut break_is_called = false;
        let mut continue_is_called = false;
        for body in body {
            if return_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterReturnDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            if break_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterBreakDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            if continue_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterContinueDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }

            match body {
                ast::IfLoopBodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, if_body_state);
                }
                ast::IfLoopBodyStatement::Binding(bind) => {
                    self.binding(bind, if_body_state);
                }
                ast::IfLoopBodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, if_body_state);
                }
                ast::IfLoopBodyStatement::If(if_condition) => {
                    self.if_condition(
                        if_condition,
                        if_body_state,
                        &Some(label_if_end.clone()),
                        Some((label_loop_start, label_loop_end)),
                    );
                }
                ast::IfLoopBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, if_body_state);
                }
                ast::IfLoopBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, if_body_state);
                    if let Some(res) = expr_result {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        if_body_state.borrow_mut().jump_function_return(res);
                        if_body_state.borrow_mut().set_return();
                        return_is_called = true;
                    }
                }
                ast::IfLoopBodyStatement::Continue => {
                    continue_is_called = true;
                    // Skip next loop  step and jump to the start
                    // of loop
                    if_body_state.borrow_mut().jump_to(label_loop_start.clone());
                }
                ast::IfLoopBodyStatement::Break => {
                    break_is_called = true;
                    // Break loop and jump to the end of loop
                    if_body_state.borrow_mut().jump_to(label_loop_end.clone());
                }
            }
        }
        return_is_called
    }

    /// # If conditions calculations
    /// Calculate conditions for if-condition. It can contain
    /// simple and logic conditions.
    pub fn if_condition_calculation(
        &mut self,
        condition: &ast::IfCondition<'_>,
        if_body_state: &Rc<RefCell<BlockState>>,
        label_if_begin: &LabelName,
        label_if_else: &LabelName,
        label_if_end: &LabelName,
        is_else: bool,
    ) {
        // Analyse if-conditions
        match condition {
            // if condition represented just as expression
            ast::IfCondition::Single(expr) => {
                // Calculate expression for single if-condition expression
                let Some(expr_result) = self.expression(expr, if_body_state) else {
                    return;
                };

                // State for if-condition from expression and if-body start
                if is_else {
                    if_body_state.borrow_mut().if_condition_expression(
                        expr_result,
                        label_if_begin.clone(),
                        label_if_else.clone(),
                    );
                } else {
                    if_body_state.borrow_mut().if_condition_expression(
                        expr_result,
                        label_if_begin.clone(),
                        label_if_end.clone(),
                    );
                }
            }
            // If condition contains logic condition expression
            ast::IfCondition::Logic(expr_logic) => {
                // Analyse if-condition logic
                let result_register = self.condition_expression(expr_logic, if_body_state);
                // State for if-condition-logic with if-body start
                if is_else {
                    if_body_state.borrow_mut().if_condition_logic(
                        label_if_begin.clone(),
                        label_if_else.clone(),
                        result_register,
                    );
                } else {
                    if_body_state.borrow_mut().if_condition_logic(
                        label_if_begin.clone(),
                        label_if_end.clone(),
                        result_register,
                    );
                }
            }
        }
    }

    /// # If-condition
    /// Analyzing includes all variants for if statements:
    /// 1. if
    /// 2. if-else
    /// 3. if-else-if
    /// It creates own state, with parent function-state. in that case
    /// if-state independent from parent state, but csn get access to
    /// parent state.
    /// If condition can't contain `else` and `if-else` on the
    /// same time.
    ///
    /// Special case for `label_end` - it should be set from previous
    /// context, and main goal is to end all of if-condition nodes in
    /// the same flow with same `if-end` label. It's especially important
    /// for `else-if` condition.
    ///
    /// ## Panics
    /// `label_loop` is must be set, it's special case for the Loop,
    /// when `label_loop` should always be set. If it doesn't set, it's
    /// unexpected behavior and program algorithm error
    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'_>,
        function_body_state: &Rc<RefCell<BlockState>>,
        label_end: &Option<LabelName>,
        label_loop: Option<(&LabelName, &LabelName)>,
    ) {
        // It can't contain `else` and `if-else` on the same time
        if let (Some(_), Some(stm)) = (&data.else_statement, &data.else_if_statement) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::IfElseDuplicated,
                String::from("if-condition"),
                stm.location(),
            ));
        }
        // Create state for if-body, from parent function state because
        // if-state can contain sub-state, that can be independent from parent
        // state
        let if_body_state = Rc::new(RefCell::new(BlockState::new(Some(
            function_body_state.clone(),
        ))));
        function_body_state
            .borrow_mut()
            .set_child(if_body_state.clone());
        // Get labels name for if-begin, and if-end
        let label_if_begin = if_body_state
            .borrow_mut()
            .get_and_set_next_label(&"if_begin".to_string().into());
        let label_if_else = if_body_state
            .borrow_mut()
            .get_and_set_next_label(&"if_else".to_string().into());
        // Set if-end label from previous context
        let label_if_end = label_end.clone().map_or_else(
            || {
                if_body_state
                    .borrow_mut()
                    .get_and_set_next_label(&"if_end".to_string().into())
            },
            |label| label,
        );
        // To set if-end as single return point check is it previously set
        let is_set_label_if_end = label_end.is_some();
        let is_else = data.else_statement.is_some() || data.else_if_statement.is_some();

        // Analyse if-conditions
        self.if_condition_calculation(
            &data.condition,
            &if_body_state,
            &label_if_begin,
            &label_if_else,
            &label_if_end,
            is_else,
        );

        //== If condition main body
        // Set if-begin label
        if_body_state.borrow_mut().set_label(label_if_begin);
        // Analyze if-conditions body kind.
        // Return flag for current body state, excluding children return claims
        let return_is_called = match &data.body {
            ast::IfBodyStatements::If(body) => {
                // Analyze if-statement body
                self.if_condition_body(body, &if_body_state, &label_if_end, label_loop)
            }
            ast::IfBodyStatements::Loop(body) => {
                // It's special case for the Loop, when `label_loop` should always be set.
                // If it doesn't set, it's unexpected behavior and program algorithm error
                let (label_loop_start, label_loop_end) =
                    label_loop.expect("loop label should be set");
                // Analyze if-loop-statement body
                self.if_condition_loop_body(
                    body,
                    &if_body_state,
                    &label_if_end,
                    label_loop_start,
                    label_loop_end,
                )
            }
        };
        // Codegen for jump to if-end statement - return to program flow.
        // If return is set do not add jump-to-end label.
        if !return_is_called {
            if_body_state.borrow_mut().jump_to(label_if_end.clone());
        }

        // Check else statements: else, else-if
        if is_else {
            // Set if-else label
            if_body_state.borrow_mut().set_label(label_if_else);

            // Analyse if-else body: data.else_statement
            if let Some(else_body) = &data.else_statement {
                // if-else has own state, different from if-state
                let if_else_body_state = Rc::new(RefCell::new(BlockState::new(Some(
                    function_body_state.clone(),
                ))));
                function_body_state
                    .borrow_mut()
                    .set_child(if_else_body_state.clone());

                let return_is_called = match else_body {
                    ast::IfBodyStatements::If(body) => {
                        // Analyze if-statement body
                        self.if_condition_body(body, &if_else_body_state, &label_if_end, label_loop)
                    }
                    ast::IfBodyStatements::Loop(body) => {
                        let (label_loop_start, label_loop_end) =
                            label_loop.expect("label should be set");
                        // Analyze if-loop-statement body
                        self.if_condition_loop_body(
                            body,
                            &if_else_body_state,
                            &label_if_end,
                            label_loop_start,
                            label_loop_end,
                        )
                    }
                };

                // Codegen for jump to if-end statement -return to program flow
                // If return is set do not add jump-to-end label.
                if !return_is_called {
                    if_body_state.borrow_mut().jump_to(label_if_end.clone());
                }
            } else if let Some(else_if_statement) = &data.else_if_statement {
                // Analyse  else-if statement
                // Set `label_if_end` to indicate single if-end point
                self.if_condition(
                    else_if_statement,
                    function_body_state,
                    &Some(label_if_end.clone()),
                    label_loop,
                );
            }
        }

        // End label for all if statement, should be set only once
        if !is_set_label_if_end {
            if_body_state.borrow_mut().set_label(label_if_end);
        }
    }

    /// # Loop
    /// Loop statement contains logic:
    /// - jump to loop
    /// - loop body
    /// - end of loop
    /// - return, break, continue
    pub fn loop_statement(
        &mut self,
        data: &[ast::LoopBodyStatement<'_>],
        function_body_state: &Rc<RefCell<BlockState>>,
    ) {
        // Create state for loop-body, from parent func state because
        // loop-state can contain sub-state, that can be independent from parent
        // state
        let loop_body_state = Rc::new(RefCell::new(BlockState::new(Some(
            function_body_state.clone(),
        ))));
        function_body_state
            .borrow_mut()
            .set_child(loop_body_state.clone());
        // Get labels name for loop-begin, and loop-end
        let label_loop_begin = loop_body_state
            .borrow_mut()
            .get_and_set_next_label(&"loop_begin".to_string().into());

        let label_loop_end = loop_body_state
            .borrow_mut()
            .get_and_set_next_label(&"loop_end".to_string().into());

        loop_body_state
            .borrow_mut()
            .jump_to(label_loop_begin.clone());
        loop_body_state
            .borrow_mut()
            .set_label(label_loop_begin.clone());

        let mut return_is_called = false;
        let mut break_is_called = false;
        let mut continue_is_called = false;
        for body in data {
            if return_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterReturnDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            if break_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterBreakDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }
            if continue_is_called {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ForbiddenCodeAfterContinueDeprecated,
                    format!("{body:?}"),
                    CodeLocation::new(1, 1),
                ));
            }

            match body {
                ast::LoopBodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &loop_body_state);
                }
                ast::LoopBodyStatement::Binding(bind) => {
                    self.binding(bind, &loop_body_state);
                }
                ast::LoopBodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, &loop_body_state);
                }
                ast::LoopBodyStatement::If(if_condition) => self.if_condition(
                    if_condition,
                    &loop_body_state,
                    &None,
                    Some((&label_loop_begin, &label_loop_end)),
                ),
                ast::LoopBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &loop_body_state);
                }
                ast::LoopBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &loop_body_state);
                    if let Some(res) = expr_result {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        loop_body_state.borrow_mut().jump_function_return(res);
                        loop_body_state.borrow_mut().set_return();
                        return_is_called = true;
                    }
                }
                ast::LoopBodyStatement::Break => {
                    // Break loop and jump to the end of loop
                    loop_body_state.borrow_mut().jump_to(label_loop_end.clone());
                    break_is_called = true;
                }
                ast::LoopBodyStatement::Continue => {
                    // Skip next loop  step and jump to the start
                    // of loop
                    loop_body_state
                        .borrow_mut()
                        .jump_to(label_loop_begin.clone());
                    continue_is_called = true;
                }
            }
        }

        // If return is called do not set loop-specific instructions
        if !return_is_called {
            // Because it's loop jump to loop begin
            loop_body_state
                .borrow_mut()
                .jump_to(label_loop_begin.clone());

            // Loop ending
            loop_body_state.borrow_mut().set_label(label_loop_end);
        }
    }

    #[allow(clippy::doc_markdown)]
    /// ## Expression
    /// Is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    /// Expressions folded by operations priority. For that
    /// expressions tree folded each leaf of tree by priority operation
    /// level. The most striking image is bracketing an expression with
    /// a higher priority, and build tree based on that.
    ///
    /// ## Return
    /// `PrimitiveValue` | `TmpRegister`
    ///
    ///  Possible algorithm conditions:
    ///     1. PrimitiveValue -> PrimitiveValue
    ///     2. Value -> load -> TmpRegister
    ///     3. FuncCall -> call -> TmpRegister
    ///     4. Operations
    ///         4.1. PrimitiveValue
    ///         - PrimitiveValue -> tmp = OP val1, val2 -> TmpRegister
    ///         - Value -> tmp1 = load -> OP val1, tmp1 -> TmpRegister
    ///         - FuncCAll -> tmp1 = call -> OP val1, tmp1 -> TmpRegister
    ///         4.2. TmpRegister (with name tmp1)
    ///         - PrimitiveValue -> tmp2 = OP tmp1, val1 -> TmpRegister
    ///         - Value -> tmp2 = load -> tmp3 = OP tmp1, tmp2 -> TmpRegister
    ///         - FuncCall -> tmp2 = call ->  tmp3 = OP tmp1, tmp2 -> TmpRegister
    ///         4.3. Operations -> recursively invoke 4.2.
    pub fn expression(
        &mut self,
        data: &ast::Expression<'_>,
        body_state: &Rc<RefCell<BlockState>>,
    ) -> Option<ExpressionResult> {
        // Fold expression operations priority
        let expr = Self::expression_operations_priority(data.clone());
        // To analyze expression first time, we set:
        // left_value - as None
        // operation - as None
        // And basic expression value is `right_value`, because
        // it can contain sub-operations (`left_value` don't contain
        // and contain Expression result)
        self.expression_operation(None, &expr, None, body_state)
    }

    /// Expression operation semantic logic:
    /// `OP(lhs, rhs)`
    /// Left-value contains optional Expression result for left side
    /// of expression.
    #[allow(clippy::too_many_lines)]
    pub fn expression_operation(
        &mut self,
        left_value: Option<&ExpressionResult>,
        right_expression: &ast::Expression<'_>,
        op: Option<&ast::ExpressionOperations>,
        body_state: &Rc<RefCell<BlockState>>,
    ) -> Option<ExpressionResult> {
        // Get right side value from expression.
        // If expression return error immediately return error
        // because next analyzer should use success result.
        let right_value = match &right_expression.expression_value {
            // Check is expression Value entity
            ast::ExpressionValue::ValueName(value) => {
                // Get value from block state
                let value_from_state = body_state.borrow_mut().get_value_name(&value.name().into());
                // Register contains result
                body_state.borrow_mut().inc_register();
                let last_register_number = body_state.borrow().last_register_number;
                // First check value in body state
                let ty = if let Some(val) = value_from_state {
                    body_state
                        .borrow_mut()
                        .expression_value(val.clone(), last_register_number);
                    val.inner_type
                } else if let Some(const_val) = self.global.constants.get(&value.name().into()) {
                    body_state
                        .borrow_mut()
                        .expression_const(const_val.clone(), last_register_number);
                    const_val.constant_type.clone()
                } else {
                    // If value doesn't exist in State or as Constant
                    self.add_error(error::StateErrorResult::new(
                        error::StateErrorKind::ValueNotFound,
                        value.name(),
                        value.location(),
                    ));
                    return None;
                };
                // Return result as register
                ExpressionResult {
                    expr_type: ty,
                    expr_value: ExpressionResultValue::Register(
                        body_state.borrow().last_register_number,
                    ),
                }
            }
            // Check is expression primitive value
            ast::ExpressionValue::PrimitiveValue(value) => {
                // Just return primitive value itself
                ExpressionResult {
                    expr_type: value.get_type().into(),
                    expr_value: ExpressionResultValue::PrimitiveValue(value.clone().into()),
                }
            }
            // Check is expression Function call entity
            ast::ExpressionValue::FunctionCall(fn_call) => {
                // We shouldn't increment register, because it's
                // inside `self.function_call`.
                // And result of function always stored in register.
                let func_call_ty = self.function_call(fn_call, body_state)?;
                // Return result as register
                body_state.borrow_mut().inc_register();
                ExpressionResult {
                    expr_type: func_call_ty,
                    expr_value: ExpressionResultValue::Register(
                        body_state.borrow().last_register_number,
                    ),
                }
            }
            ast::ExpressionValue::StructValue(value) => {
                let struct_value: ExpressionStructValue = value.clone().into();
                // Can be only Value from state, not constant
                // Get value from block state
                let val = body_state
                    .borrow_mut()
                    .get_value_name(&struct_value.name)
                    .or_else(|| {
                        // If value doesn't exist
                        self.add_error(error::StateErrorResult::new(
                            error::StateErrorKind::ValueNotFound,
                            value.name.name(),
                            value.name.location(),
                        ));
                        None
                    })?;
                // Check is value type is struct
                let ty = val.inner_type.get_struct().or_else(|| {
                    self.add_error(error::StateErrorResult::new(
                        error::StateErrorKind::ValueNotStruct,
                        value.name.name(),
                        value.name.location(),
                    ));
                    None
                })?;
                // Check is type exists
                if !self.check_type_exists(&val.inner_type, &value.name.name(), &value.name) {
                    return None;
                }
                if &Type::Struct(ty.clone()) != self.global.types.get(&val.inner_type.name())? {
                    self.add_error(error::StateErrorResult::new(
                        error::StateErrorKind::WrongExpressionType,
                        value.name.name(),
                        value.name.location(),
                    ));
                    return None;
                }

                let attributes = ty
                    .attributes
                    .get(&struct_value.attribute)
                    .or_else(|| {
                        self.add_error(error::StateErrorResult::new(
                            error::StateErrorKind::ValueNotStructField,
                            value.name.name(),
                            value.name.location(),
                        ));
                        None
                    })?
                    .clone();

                // Register contains result
                body_state.borrow_mut().inc_register();
                let last_register_number = body_state.borrow().last_register_number;
                body_state.borrow_mut().expression_struct_value(
                    val.clone(),
                    attributes.attr_index,
                    last_register_number,
                );

                body_state.borrow_mut().inc_register();
                ExpressionResult {
                    expr_type: attributes.attr_type,
                    expr_value: ExpressionResultValue::Register(
                        body_state.borrow().last_register_number,
                    ),
                }
            }
            ast::ExpressionValue::Expression(expr) => {
                // Subexpression should be analyzed independently
                self.expression(expr, body_state)?
            }
        };
        // Check left expression side and generate expression operation code
        let expression_result = if let (Some(left_value), Some(op)) = (left_value, op) {
            if left_value.expr_type != right_value.expr_type {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::WrongExpressionType,
                    left_value.expr_type.to_string(),
                    right_expression.location(),
                ));
                // Do not fetch other expression flow if type is wrong
                return None;
            }
            // Expression operation is set to register
            body_state.borrow_mut().inc_register();
            let last_register_number = body_state.borrow().last_register_number;
            // Call expression operation for: OP(left_value, right_value)
            body_state.borrow_mut().expression_operation(
                op.clone().into(),
                left_value.clone(),
                right_value.clone(),
                last_register_number,
            );
            // Expression result value  for Operations is always should be "register"
            ExpressionResult {
                expr_type: right_value.expr_type,
                expr_value: ExpressionResultValue::Register(
                    body_state.borrow().last_register_number,
                ),
            }
        } else {
            right_value
        };

        // Check is for right value contain next operation
        if let Some((operation, expr)) = &right_expression.operation {
            // Recursively call, where current Execution result set as left
            // side expressionf
            self.expression_operation(Some(&expression_result), expr, Some(operation), body_state)
        } else {
            Some(expression_result)
        }
    }

    /// # Expression operation priority
    /// Fold expression priority.
    /// Pass expressions tree from max priority level to minimum
    /// priority level. If expression priority for concrete branch
    /// founded, it's folded to leaf (same as bracketing).
    ///
    /// ## Return
    /// New folded expressions tree.
    fn expression_operations_priority(data: ast::Expression<'_>) -> ast::Expression<'_> {
        let mut data = data;
        for priority in (0..=MAX_PRIORITY_LEVEL_FOR_EXPRESSIONS).rev() {
            data = Self::fetch_op_priority(data, priority);
        }
        data
    }

    /// Fetch expression operation priories and fold it.
    /// Expressions folded by operations priority. For that expressions
    /// tree folded each branch of tree to leaf by priority operation
    /// level. The most striking image is bracketing an expression with
    /// a higher priority, and build tree based on that.
    ///
    /// For example: expr = expr1 OP1 expr2 - it has 2 branches
    /// if expr2 contain subbranch (for example: `expr2 OP2 expr3`) we trying
    /// to find priority level for current pass. And if `priority_level == OP1`
    /// - fold it to leaf.
    /// NOTICE: expr1 can't contain subbranches by design. So we pass
    /// expression tree from left to right.
    /// If priority level not equal, we just return income expression, or
    /// if it has subbranch - launch fetching subbranch
    fn fetch_op_priority(data: ast::Expression<'_>, priority_level: u8) -> ast::Expression<'_> {
        // Check is expression contains right side with operation
        if let Some((op, expr)) = data.clone().operation {
            // Check is right expression contain subbranch (sub operation)
            if let Some((next_op, next_expr)) = expr.operation.clone() {
                // Check incoming expression operation priority level
                if op.priority() == priority_level {
                    // Fold expression to leaf - creating new expression as value
                    let expression_value =
                        ast::ExpressionValue::Expression(Box::new(ast::Expression {
                            expression_value: data.expression_value,
                            operation: Some((
                                op,
                                Box::new(ast::Expression {
                                    expression_value: expr.expression_value,
                                    operation: None,
                                }),
                            )),
                        }));
                    // Fetch next expression branch
                    let new_expr = Self::fetch_op_priority(*next_expr, priority_level);
                    // Create new expression with folded `expression_value`
                    ast::Expression {
                        expression_value,
                        operation: Some((next_op, Box::new(new_expr))),
                    }
                } else {
                    // If priority not equal for current level just
                    // fetch right side of expression for next branches
                    let new_expr =
                        if next_op.priority() > op.priority() && next_expr.operation.is_none() {
                            // Pack expression to leaf
                            ast::Expression {
                                expression_value: ast::ExpressionValue::Expression(expr),
                                operation: None,
                            }
                        } else {
                            Self::fetch_op_priority(*expr, priority_level)
                        };
                    // Rebuild expression tree
                    ast::Expression {
                        expression_value: data.expression_value,
                        operation: Some((op, Box::new(new_expr))),
                    }
                }
            } else {
                data
            }
        } else {
            data
        }
    }
}
