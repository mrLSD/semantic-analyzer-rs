//! # Semantic analyzer
//! Semantic analyzer provide algorithms to analyze AST for different
//! rules and generate `Codegen`. AST represent tree n0des of language
//! constructions and fully cover all flow of the program represented through
//! AST.
//!
//! Semantic contains basic entities:
//! - `Global State` - global state of semantic analyzer.
//! - `Block State` - state for functions and sub blocks for it.
//! - `Codegen` - generated code as result of semantic analyzing.
//!
//! Codegen is result of Semantic analyzer and contains prepared data tree
//! of generated code for next step - compilation generated raw program code.
use crate::ast::{self, GetName};
use crate::codegen::Codegen;
use crate::types::error::{self, EmptyError};
use crate::types::{
    Constant, ConstantName, ExpressionResult, ExpressionResultValue, Function, FunctionName,
    InnerType, InnerValueName, LabelName, StateResult, Value, ValueName,
};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

const IF_BEGIN: &str = "if_begin";
const IF_END: &str = "if_end";
const IF_ELSE: &str = "if_else";
const LOOP_BEGIN: &str = "loop_begin";
const LOOP_END: &str = "loop_end";

/// # Block state
/// - `values` - contains unique values map for current state but not unique
///   for parent states. The map contains key-value: `value_name` (unique
///   only for current state); and `Value` itself - value parameters.   
/// - `inner_values_name` - is entity that represent inner value name - it
///   can be different from `Value` name because it should be unique for all
///   parent states. For example, of 3 values with name `x`, inner value
///   name will be: [`x`, `x.0`, `x.1`]. It mean, inner value name can
///   contain `value counter` as end of the name.
/// - `labels` - labels set, for conditional operation. Unique for current
///   and all paren states.
/// - `last_register_number` - represent register counter for current and
///   all parent states for `Codegen`. Register represented as `u64` and
///   should be linearly incremented.
/// - `manual_return` - flag indicated, that return was invoked from
/// other state, for example: if-flow, loop-flow
/// - `parent` - represent parent states.  
#[derive(Debug)]
pub struct BlockState {
    /// State values
    pub values: HashMap<ValueName, Value>,
    /// Used to keep all names in the block state (and parent) as unique
    pub inner_values_name: HashSet<InnerValueName>,
    /// State labels for conditional operations
    pub labels: HashSet<LabelName>,
    /// Last register for unique register representation
    pub last_register_number: u64,
    /// Manual return from other states
    pub manual_return: bool,
    /// Parent state
    pub parent: Option<Rc<RefCell<BlockState>>>,
}

impl BlockState {
    /// Init block state with optional `parent` state
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        // Get values from parent
        let (last_register_number, inner_values_name, labels, manual_return) =
            parent.clone().map_or_else(
                || (0, HashSet::new(), HashSet::new(), false),
                |p| {
                    let parent = p.borrow();
                    (
                        parent.last_register_number,
                        parent.inner_values_name.clone(),
                        parent.labels.clone(),
                        parent.manual_return,
                    )
                },
            );
        Self {
            values: HashMap::new(),
            inner_values_name,
            labels,
            last_register_number,
            manual_return,
            parent,
        }
    }

    /// Set `last_register_number` for current and parent states
    fn set_register(&mut self, last_register_number: u64) {
        self.last_register_number = last_register_number;
        // Set `last_register_number` for parents
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_register(last_register_number);
        }
    }

    /// Increment register
    fn inc_register(&mut self) {
        self.set_register(self.last_register_number + 1);
    }

    /// Set value inner name to current state and parent states
    fn set_inner_value_name(&mut self, name: &InnerValueName) {
        self.inner_values_name.insert(name.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_inner_value_name(name);
        }
    }

    /// Check is `inner_value_name` exist in current and parent states
    fn is_inner_value_name_exist(&self, name: &InnerValueName) -> bool {
        if self.inner_values_name.contains(name) {
            return true;
        } else if let Some(parent) = &self.parent {
            return parent.borrow().is_inner_value_name_exist(name);
        }
        false
    }

    /// Get `Value` by value name from current state.
    /// If not found on current state - recursively find in parent states.
    fn get_value_name(&self, name: &ValueName) -> Option<Value> {
        if let Some(val) = self.values.get(name) {
            return Some(val.clone());
        } else if let Some(parent) = &self.parent {
            return parent.borrow().get_value_name(name);
        }
        None
    }

    /// Check is label name exist in current and parent states
    fn is_label_name_exist(&self, name: &LabelName) -> bool {
        if self.labels.contains(name) {
            return true;
        } else if let Some(parent) = &self.parent {
            return parent.borrow().is_label_name_exist(name);
        }
        false
    }

    /// Set label name to current and all parent states
    fn set_label_name(&mut self, name: &LabelName) {
        self.labels.insert(name.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_label_name(name);
        }
    }

    /// Set attribute counter - increment, if counter exist.
    fn set_attr_counter(val: &str) -> String {
        let val_attr: Vec<&str> = val.split('.').collect();
        if val_attr.len() == 2 {
            let i: u64 = val_attr[1].parse().expect("expect integer");
            format!("{}.{:?}", val_attr[0], i + 1)
        } else {
            format!("{}.0", val_attr[0])
        }
    }

    /// Get and set next label for condition operations
    /// - If label doesn't exist in State - just insert to State and
    ///   self return
    /// - if label exists, get label counter
    fn get_and_set_next_label(&mut self, label: &LabelName) -> LabelName {
        // Check is label exists. If doesn't set it to State and return self
        if !self.is_label_name_exist(label) {
            self.set_label_name(label);
            return label.clone();
        }
        // If label exists, split and get number of label counter
        let name: LabelName = Self::set_attr_counter(&label.to_string()).into();
        if self.is_label_name_exist(&name) {
            self.get_and_set_next_label(&name)
        } else {
            self.set_label_name(&name);
            name
        }
    }

    /// Get next `inner_value_name` by name counter for current and
    /// parent states. The `inner_value_name` should always be unique.
    fn get_next_inner_name(&self, val: &InnerValueName) -> InnerValueName {
        // Increment inner value name counter for shadowed variable
        let name: InnerValueName = Self::set_attr_counter(&val.to_string()).into();
        if self.is_inner_value_name_exist(&name) {
            self.get_next_inner_name(&name)
        } else {
            name
        }
    }

    /// Set return status flag for current and parent states
    fn set_return(&mut self) {
        self.manual_return = true;
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_return();
        }
    }
}

/// # Global State
/// Global state can contains state of:
/// - Constants
/// - Types
/// - Functions
/// The visibility of Global state limited by
/// current module.
#[derive(Debug)]
pub struct GlobalState {
    pub constants: HashMap<ConstantName, Constant>,
    pub types: HashSet<InnerType>,
    pub functions: HashMap<FunctionName, Function>,
}

/// # State
/// Basic entity that contains `Global State`
/// and `Codegen` tree.
/// - errors - State analyzing errors
#[derive(Debug)]
pub struct State<T: Codegen> {
    pub global: GlobalState,
    pub codegen: T,
    pub errors: Vec<error::StateErrorResult>,
}

impl<T: Codegen<Backend = T>> State<T> {
    /// Init new `State`
    pub fn new(codegen: T) -> Self {
        Self {
            global: GlobalState {
                functions: HashMap::new(),
                types: HashSet::new(),
                constants: HashMap::new(),
            },
            codegen,
            errors: Vec::new(),
        }
    }

    fn add_error(&mut self, err: error::StateErrorResult) {
        self.errors.push(err);
    }

    /// Run semantic analyzer that covers all flow
    pub fn run(&mut self, data: &ast::Main<'_>) -> StateResult<()> {
        // Execute each kind of analyzing and return errors data.
        // For functions - fetch only declaration for fast-forward
        // identification for using it in functions body.
        for main in data.iter() {
            match main {
                ast::MainStatement::Import(import) => self.import(import),
                ast::MainStatement::Constant(constant) => self.constant(constant),
                ast::MainStatement::Types(types) => self.types(types),
                ast::MainStatement::Function(function) => self.function_declaration(function),
            }
        }
        // After getting all functions declarations, fetch only functions body
        for main in data.iter() {
            if let ast::MainStatement::Function(function) = main {
                self.function_body(function);
            }
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(error::StateError(error::StateErrorKind::Common))
        }
    }

    /// Import analyzer
    #[allow(clippy::unused_self, clippy::unnecessary_wraps)]
    pub const fn import(&self, _data: &ast::ImportPath<'_>) {}

    /// Types declaration analyzer. Add types to Global State.
    pub fn types(&mut self, data: &ast::StructTypes<'_>) {
        if self.global.types.contains(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::TypeAlreadyExist,
                data.name(),
                0,
                0,
            ));
            return;
        }
        self.global.types.insert(data.name().into());
        self.codegen.types(data);
    }

    /// Constant analyzer. Add it got Global State
    pub fn constant(&mut self, data: &ast::Constant<'_>) {
        if self.global.constants.contains_key(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ConstantAlreadyExist,
                data.name(),
                0,
                0,
            ));
            return;
        }
        self.global.constants.insert(
            data.name().into(),
            Constant {
                name: data.name().into(),
                inner_type: data.constant_type.name().into(),
            },
        );
        self.codegen.constant(data);
    }

    /// Function declaration analyze. Add it to Global State/
    pub fn function_declaration(&mut self, data: &ast::FunctionStatement<'_>) {
        if self.global.functions.contains_key(&data.name().into()) {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::FunctionAlreadyExist,
                data.name(),
                0,
                0,
            ));
            return;
        }
        self.global.functions.insert(
            data.name().into(),
            Function {
                inner_name: data.name().into(),
                inner_type: data.result_type.name().into(),
                parameters: data
                    .parameters
                    .iter()
                    .map(|p| p.parameter_type.name().into())
                    .collect(),
            },
        );
        self.codegen.function_declaration(data);
    }

    /// Function body analyze.
    /// It is basic execution entity for program flow.
    /// It's operate sub analyze for function elements. It's contain
    /// Body State for current and child states.
    pub fn function_body(&mut self, data: &ast::FunctionStatement<'_>) {
        // Init empty function body state
        let body_state = Rc::new(RefCell::new(BlockState::new(None)));
        // Flag to indicate is function return called
        let mut return_is_called = false;
        // Fetch function elements and gather errors
        for body in &data.body {
            match body {
                ast::BodyStatement::LetBinding(bind) => {
                    let _ = self.let_binding(bind, &body_state);
                }
                ast::BodyStatement::Binding(bind) => {
                    let _ = self.binding(bind, &body_state);
                }
                ast::BodyStatement::FunctionCall(fn_call) => {
                    let _ = self.function_call(fn_call, &body_state);
                }
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state, None, None);
                }
                ast::BodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &body_state);
                }
                ast::BodyStatement::Expression(expression) => {
                    let expr_result = self.expression(expression, &body_state);
                    let _ = expr_result.map(|res| {
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
                            self.codegen.expression_function_return_with_label(&res);
                        } else {
                            self.codegen.expression_function_return(&res);
                        }
                    });
                }
                ast::BodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &body_state);
                    let _ = expr_result.map(|res| {
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
                            self.codegen.expression_function_return_with_label(&res);
                        } else {
                            self.codegen.expression_function_return(&res);
                        }
                    });
                }
            }
        }
        // Check is function contain return
        if !return_is_called {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ReturnNotFound,
                String::new(),
                0,
                0,
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
        state: &Rc<RefCell<BlockState>>,
    ) -> Result<(), EmptyError> {
        // Call value analytics before putting let-value to state
        let expr_result = self.expression(&data.value, state)?;

        if let Some(ty) = data.clone().value_type {
            if expr_result.expr_type != ty.name().into() {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::WrongLetType,
                    data.name(),
                    0,
                    0,
                ));
                return Err(EmptyError);
            }
        }
        let let_ty = expr_result.clone().expr_type;

        // Find value in current state and parent states
        let value = state.borrow().get_value_name(&data.name().into());
        // Calculate `inner_name` as unique for current and all parent states
        let inner_name = value.map_or_else(
            || {
                // if value not found in all states check and set
                // `inner_value` from value name
                state.borrow().get_next_inner_name(&data.name().into())
            },
            |val| {
                // Increment inner value name counter for shadowed variable
                // and check variable inner_name for and inner_values in current state
                state.borrow().get_next_inner_name(&val.inner_name)
            },
        );
        // Set value parameters
        let value = Value {
            inner_name: inner_name.clone(),
            inner_type: let_ty,
            mutable: data.mutable,
            alloca: false,
            malloc: false,
        };
        // Value inserted only to current state by Value name and Value data
        state
            .borrow_mut()
            .values
            .insert(data.name().into(), value.clone());
        // Set `inner_name` to current state and all parent states
        state.borrow_mut().set_inner_value_name(&inner_name);

        self.codegen.let_binding(&value, &expr_result);
        Ok(())
    }

    /// # Binding statement
    /// Analyze binding statement for mutable variables:
    /// 1. Bind from expression. First should be analysed
    ///    `expression` for binding value.
    /// 2. Read value for current state.
    /// 3. Update value to current values state map: value `name` -> `Data`
    /// 4. Codegen with Store action
    pub fn binding(
        &mut self,
        data: &ast::Binding<'_>,
        state: &Rc<RefCell<BlockState>>,
    ) -> Result<(), EmptyError> {
        // Call value analytics before putting let-value to state
        let expr_result = self.expression(&data.value, state)?;

        // Find value in current state and parent states
        let value = state
            .borrow()
            .get_value_name(&data.name().into())
            .ok_or_else(|| {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::ValueNotFound,
                    data.name(),
                    0,
                    0,
                ));
                EmptyError
            })?;
        // Check is value mutable
        if !value.mutable {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::ValueIsNotMutable,
                data.name(),
                0,
                0,
            ));
            return Err(EmptyError);
        }

        self.codegen.binding(&value, &expr_result);
        Ok(())
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
    ) -> Result<InnerType, EmptyError> {
        // Check is function exists in global functions stat
        let Some(func_data) = self.global.functions.get(&data.name().into()).cloned() else {
            self.add_error(error::StateErrorResult::new(
                 error::StateErrorKind::FunctionNotFound,
                 data.name(),
                 0,
                 0,
             ));
            return Err(EmptyError);
        };
        let fn_type = func_data.inner_type.clone();

        // Analyse function parameters expressions, check their types
        // and set result to array
        let mut params: Vec<ExpressionResult> = vec![];
        for (i, expr) in data.parameters.iter().enumerate() {
            let expr_result = self.expression(expr, body_state)?;
            if expr_result.expr_type != func_data.parameters[i] {
                self.add_error(error::StateErrorResult::new(
                    error::StateErrorKind::FunctionParameterTypeWrong,
                    expr_result.expr_type.to_string(),
                    0,
                    0,
                ));
                continue;
            }
            params.push(expr_result);
        }

        // Codegen for function-call
        body_state.borrow_mut().inc_register();
        // Store always result to register even for void result
        self.codegen
            .call(&func_data, params, body_state.borrow().last_register_number);
        Ok(fn_type)
    }

    /// # condition-expression
    /// Analyse condition operations.    
    pub fn condition_expression(
        &mut self,
        data: &ast::ExpressionLogicCondition<'_>,
        function_body_state: &Rc<RefCell<BlockState>>,
    ) -> Result<(), EmptyError> {
        // Analyse left expression of left condition
        let left_expr = &data.left.left;
        let left_res = self.expression(left_expr, function_body_state);

        // Analyse right expression of left condition
        let right_expr = &data.left.right;
        let right_res = self.expression(right_expr, function_body_state);

        // Unwrap result only after analysing
        let left_res = left_res?;
        let right_res = right_res?;

        // Increase register counter before generate condition
        function_body_state.borrow_mut().inc_register();
        // Codegen for left condition and set result to register
        self.codegen.condition_expression(
            &left_res,
            &right_res,
            &data.left.condition,
            function_body_state.borrow().last_register_number,
        );

        // Analyze right condition
        if let Some(right) = &data.right {
            // Get register form left operation
            let left_register_number = function_body_state.borrow().last_register_number;
            // Analyse recursively right part of condition
            self.condition_expression(&right.1, function_body_state)?;

            // Get register form right operation of right side analyzing
            let right_register_number = function_body_state.borrow().last_register_number;
            // Increase register counter before generate logic condition
            function_body_state.borrow_mut().inc_register();

            // Codegen for logical condition for: left [LOGIC-OP] right
            // The result generated from registers, and stored to
            // new register
            self.codegen.logic_condition(
                left_register_number,
                right_register_number,
                &right.0,
                function_body_state.borrow().last_register_number,
            );
        }

        Ok(())
    }

    /// # If-condition body
    /// Analyze body for ant if condition:
    /// - if, else, if-else
    pub fn if_condition_body(
        &mut self,
        body: &[ast::IfBodyStatement<'_>],
        if_body_state: &Rc<RefCell<BlockState>>,
    ) {
        for body in body.iter() {
            match body {
                ast::IfBodyStatement::LetBinding(bind) => {
                    let _ = self.let_binding(bind, if_body_state);
                }
                ast::IfBodyStatement::Binding(bind) => {
                    let _ = self.binding(bind, if_body_state);
                }
                ast::IfBodyStatement::FunctionCall(fn_call) => {
                    let _ = self.function_call(fn_call, if_body_state);
                }
                ast::IfBodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, if_body_state, None, None);
                }
                ast::IfBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, if_body_state);
                }
                ast::IfBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, if_body_state);
                    let _ = expr_result.map(|res| {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        self.codegen.jump_function_return(&res);
                        if_body_state.borrow_mut().set_return();
                    });
                }
            }
        }
    }

    /// # If-condition loop body
    /// Analyze body for ant if condition:
    /// - if, else, if-else
    #[allow(dead_code)]
    pub fn if_condition_loop_body(
        &mut self,
        body: &[ast::IfLoopBodyStatement<'_>],
        if_body_state: &Rc<RefCell<BlockState>>,
        label_loop_start: &LabelName,
        label_loop_end: &LabelName,
    ) {
        for body in body.iter() {
            match body {
                ast::IfLoopBodyStatement::LetBinding(bind) => {
                    let _ = self.let_binding(bind, if_body_state);
                }
                ast::IfLoopBodyStatement::Binding(bind) => {
                    let _ = self.binding(bind, if_body_state);
                }
                ast::IfLoopBodyStatement::FunctionCall(fn_call) => {
                    let _ = self.function_call(fn_call, if_body_state);
                }
                ast::IfLoopBodyStatement::If(if_condition) => {
                    self.if_condition(
                        if_condition,
                        if_body_state,
                        None,
                        Some((label_loop_start, label_loop_end)),
                    );
                }
                ast::IfLoopBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, if_body_state);
                }
                ast::IfLoopBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, if_body_state);
                    let _ = expr_result.map(|res| {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        self.codegen.jump_function_return(&res);
                        if_body_state.borrow_mut().set_return();
                    });
                }
                ast::IfLoopBodyStatement::Continue => {
                    // Skip next loop  step and jump to the start
                    // of loop
                    self.codegen.jump_to(label_loop_start);
                }
                ast::IfLoopBodyStatement::Break => {
                    // Break loop and jump to the end of loop
                    self.codegen.jump_to(label_loop_end);
                }
            }
        }
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
    ) -> Result<(), EmptyError> {
        // Analyse if-conditions
        match condition {
            // if condition represented just as expression
            ast::IfCondition::Single(expr) => {
                // Calculate expression for single if-condition expression
                let expr_result = self.expression(expr, if_body_state)?;
                // Codegen for if-condition from expression and if-body start
                if is_else {
                    self.codegen.if_condition_expression(
                        &expr_result,
                        label_if_begin,
                        label_if_else,
                    );
                } else {
                    self.codegen.if_condition_expression(
                        &expr_result,
                        label_if_begin,
                        label_if_end,
                    );
                }
            }
            // If condition contains logic condition expression
            ast::IfCondition::Logic(expr_logic) => {
                // Analyse if-condition logic
                let _ = self.condition_expression(expr_logic, if_body_state);
                // Codegen for if-condition-logic with if-body start
                if is_else {
                    self.codegen.if_condition_logic(
                        label_if_begin,
                        label_if_else,
                        if_body_state.borrow().last_register_number,
                    );
                } else {
                    self.codegen.if_condition_logic(
                        label_if_begin,
                        label_if_end,
                        if_body_state.borrow().last_register_number,
                    );
                }
            }
        }
        Ok(())
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
    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'_>,
        function_body_state: &Rc<RefCell<BlockState>>,
        label_end: Option<LabelName>,
        label_loop: Option<(&LabelName, &LabelName)>,
    ) {
        // It can't contain `else` and `if-else` on the same time
        if data.else_if_statement.is_some() && data.else_if_statement.is_some() {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::IfElseDuplicated,
                String::from("if-condition"),
                0,
                0,
            ));
        }
        // Create state for if-body, from parent function state because
        // if-state can contain sub-state, that can be independent from parent
        // state
        let if_body_state = Rc::new(RefCell::new(BlockState::new(Some(
            function_body_state.clone(),
        ))));

        // Get labels name for if-begin, and if-end
        let label_if_begin = if_body_state
            .borrow_mut()
            .get_and_set_next_label(&IF_BEGIN.to_string().into());
        let label_if_else = if_body_state
            .borrow_mut()
            .get_and_set_next_label(&IF_ELSE.to_string().into());
        // Set if-end label from previous context, if exist
        let label_if_end = label_end.map_or_else(
            || {
                if_body_state
                    .borrow_mut()
                    .get_and_set_next_label(&IF_END.to_string().into())
            },
            |label| label,
        );
        let is_else = data.else_if_statement.is_some() || data.else_if_statement.is_some();

        // Analyse if-conditions
        let _ = self.if_condition_calculation(
            &data.condition,
            &if_body_state,
            &label_if_begin,
            &label_if_else,
            &label_if_end,
            is_else,
        );

        //== If condition main body
        // Set if-begin label
        self.codegen.set_label(&label_if_begin);
        // Analyze if-conditions body kind
        match &data.body {
            ast::IfBodyStatements::If(body) => {
                // Analyze if-statement body
                self.if_condition_body(body, &if_body_state);
            }
            ast::IfBodyStatements::Loop(body) => {
                let (label_loop_start, label_loop_end) = label_loop.expect("label should be set");
                // Analyze if-loop-statement body
                self.if_condition_loop_body(body, &if_body_state, label_loop_start, label_loop_end);
            }
        }
        // Codegen for jump to if-end statement -return to program flow
        self.codegen.jump_to(&label_if_end);

        // Check else statements: else, else-if
        if is_else {
            // Set if-else label
            self.codegen.set_label(&label_if_else);
            // if-else has own state, different from if-state
            let if_else_body_state = Rc::new(RefCell::new(BlockState::new(Some(
                function_body_state.clone(),
            ))));
            // Analyse if-else body: data.else_statement
            if let Some(else_body) = &data.else_statement {
                match else_body {
                    ast::IfBodyStatements::If(body) => {
                        // Analyze if-statement body
                        self.if_condition_body(body, &if_else_body_state);
                    }
                    ast::IfBodyStatements::Loop(body) => {
                        let (label_loop_start, label_loop_end) =
                            label_loop.expect("label should be set");
                        // Analyze if-loop-statement body
                        self.if_condition_loop_body(
                            body,
                            &if_else_body_state,
                            label_loop_start,
                            label_loop_end,
                        );
                    }
                }

                // Codegen for jump to if-end statement -return to program flow
                self.codegen.jump_to(&label_if_end);
            } else if let Some(else_if_statement) = &data.else_if_statement {
                // Analyse  else-if statement
                self.if_condition(
                    else_if_statement,
                    function_body_state,
                    Some(label_if_end.clone()),
                    label_loop,
                );
            }
        }

        // End label for all if statement
        self.codegen.set_label(&label_if_end);
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
        // Get labels name for loop-begin, and loop-end
        let label_loop_begin = loop_body_state
            .borrow_mut()
            .get_and_set_next_label(&LOOP_BEGIN.to_string().into());

        let label_loop_end = loop_body_state
            .borrow_mut()
            .get_and_set_next_label(&LOOP_END.to_string().into());

        self.codegen.jump_to(&label_loop_begin);
        self.codegen.set_label(&label_loop_begin);

        for body in data.iter() {
            match body {
                ast::LoopBodyStatement::LetBinding(bind) => {
                    let _ = self.let_binding(bind, &loop_body_state);
                }
                ast::LoopBodyStatement::Binding(bind) => {
                    let _ = self.binding(bind, &loop_body_state).map_err(|e| vec![e]);
                }
                ast::LoopBodyStatement::FunctionCall(fn_call) => {
                    let _ = self.function_call(fn_call, &loop_body_state);
                }
                ast::LoopBodyStatement::If(if_condition) => self.if_condition(
                    if_condition,
                    &loop_body_state,
                    None,
                    Some((&label_loop_begin, &label_loop_end)),
                ),
                ast::LoopBodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &loop_body_state);
                }
                ast::LoopBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &loop_body_state);
                    let _ = expr_result.map(|res| {
                        // Jump to return label in codegen and set return
                        // status to indicate function, that it's manual
                        // return
                        self.codegen.jump_function_return(&res);
                        loop_body_state.borrow_mut().set_return();
                    });
                }
                ast::LoopBodyStatement::Break => {
                    // Break loop and jump to the end of loop
                    self.codegen.jump_to(&label_loop_end);
                }
                ast::LoopBodyStatement::Continue => {
                    // Skip next loop  step and jump to the start
                    // of loop
                    self.codegen.jump_to(&label_loop_begin);
                }
            }
        }

        // Loop ending
        self.codegen.set_label(&label_loop_end);
    }

    #[allow(clippy::doc_markdown)]
    /// ## Expression
    /// Is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    /// Return: `PrimitiveValue` | `TmpRegister`
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
    ) -> Result<ExpressionResult, EmptyError> {
        // To analyze expression first time, we set:
        // left_value - as None
        // operation - as None
        // And basic expression value is `right_value`, because
        // it can contain sub-operations (`left_value` don't contain
        // and contain Expression result)
        self.expression_operation(None, data, None, body_state)
    }

    /// Expression operation semantic logic:
    /// `OP(lhs, rhs)`
    /// Left-value contains optional Exrpression result for left side
    /// of expression.
    pub fn expression_operation(
        &mut self,
        left_value: Option<&ExpressionResult>,
        right_expression: &ast::Expression<'_>,
        op: Option<&ast::ExpressionOperations>,
        body_state: &Rc<RefCell<BlockState>>,
    ) -> Result<ExpressionResult, EmptyError> {
        // Get right side value from expression.
        // If expression return error immediately return error
        // because next analyzer should use success result.
        let right_value = match &right_expression.expression_value {
            // Check is expression Value entity
            ast::ExpressionValue::ValueName(value) => {
                // Get value from block state
                let value_from_state = body_state.borrow_mut().get_value_name(&value.name().into());
                // Check is value exist in State or as Constant
                if value_from_state.is_some()
                    || self.global.constants.contains_key(&value.name().into())
                {
                    // Increase register counter before loading value
                    body_state.borrow_mut().inc_register();
                    // First check value in body state
                    let ty = if let Some(val) = value_from_state {
                        // If it's value then Load it to register
                        self.codegen
                            .expression_value(&val, body_state.borrow().last_register_number);
                        val.inner_type
                    } else if let Some(const_val) = self.global.constants.get(&value.name().into())
                    {
                        // If value is constant load it to register
                        self.codegen
                            .expression_const(const_val, body_state.borrow().last_register_number);
                        const_val.inner_type.clone()
                    } else {
                        return Err(EmptyError);
                    };
                    // Return result as register
                    ExpressionResult {
                        expr_type: ty,
                        expr_value: ExpressionResultValue::Register(
                            body_state.borrow().last_register_number,
                        ),
                    }
                } else {
                    // If value doesn't exist
                    self.add_error(error::StateErrorResult::new(
                        error::StateErrorKind::ValueNotFound,
                        value.name(),
                        0,
                        0,
                    ));
                    return Err(EmptyError);
                }
            }
            // Check is expression primitive value
            ast::ExpressionValue::PrimitiveValue(value) => {
                // Just return primitive value itself
                ExpressionResult {
                    expr_type: value.get_type().name().into(),
                    expr_value: ExpressionResultValue::PrimitiveValue(value.clone()),
                }
            }
            // Check is expression Function call entity
            ast::ExpressionValue::FunctionCall(fn_call) => {
                // We shouldn't increment register, because it's
                // inside `self.function_call`.
                // And result of function always stored in register.
                let func_call_ty = self.function_call(fn_call, body_state)?;
                // Return result as register
                ExpressionResult {
                    expr_type: func_call_ty,
                    expr_value: ExpressionResultValue::Register(
                        body_state.borrow().last_register_number,
                    ),
                }
            }
            ast::ExpressionValue::StructValue(_value) => {
                todo!()
            }
        };
        // It's special case for "pure" expression - without operation.
        // For that check also left side of expression shouldn't exist
        if left_value.is_none() || op.is_none() {
            return Ok(right_value);
        }
        let left_value = left_value.unwrap();
        if left_value.expr_type != right_value.expr_type {
            self.add_error(error::StateErrorResult::new(
                error::StateErrorKind::WrongExpressionType,
                left_value.expr_type.to_string(),
                0,
                0,
            ));
        }
        // Call expression operation for: OP(left_value, right_value)
        // and return result of that call as register
        body_state.borrow_mut().inc_register();
        self.codegen.expression_operation(
            op.unwrap(),
            left_value,
            &right_value,
            body_state.borrow().last_register_number,
        );
        let expression_result = ExpressionResult {
            expr_type: right_value.expr_type,
            expr_value: ExpressionResultValue::Register(body_state.borrow().last_register_number),
        };

        // Check is for right value contain next operation
        if let Some((operation, expr)) = &right_expression.operation {
            // Recursively call, where current Execution result set as left
            // side expression
            self.expression_operation(Some(&expression_result), expr, Some(operation), body_state)
        } else {
            Ok(expression_result)
        }
    }
}
