use crate::ast::{self, ExpressionOperations, GetName, PrimitiveValue};
use crate::codegen::Codegen;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

const IF_BEGIN: &str = "if_begin";
const IF_END: &str = "if_end";
const _IF_ELSE: &str = "if_else";

pub type StateResult<T> = Result<T, error::StateErrorResult>;
pub type StateResults<T> = Result<T, Vec<error::StateErrorResult>>;

type ValueName = String;
type InnerType = String;

#[derive(Debug)]
pub struct Constant {
    pub name: String,
    pub inner_type: InnerType,
}

#[derive(Debug, Clone)]
pub struct Value {
    pub inner_name: ValueName,
    pub inner_type: InnerType,
    pub allocated: bool,
}

#[derive(Debug)]
pub struct ValueBlockState {
    pub values: HashMap<ValueName, Value>,
    // Used to keep all names in the block state as unique
    pub inner_values_name: HashSet<ValueName>,
    pub labels: HashSet<String>,
    pub last_register_number: u64,
    pub parent: Option<Rc<RefCell<ValueBlockState>>>,
}

impl ValueBlockState {
    pub fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        // Get last_register_number from parent
        let last_register_number = parent
            .clone()
            .map_or(0, |p| p.borrow().last_register_number);
        let inner_values_name = parent
            .clone()
            .map_or_else(HashSet::new, |p| p.borrow().inner_values_name.clone());
        let labels = parent
            .clone()
            .map_or_else(HashSet::new, |p| p.borrow().labels.clone());
        Self {
            values: HashMap::new(),
            inner_values_name,
            labels,
            last_register_number,
            parent,
        }
    }

    fn set_register(&mut self, last_register_number: u64) {
        self.last_register_number = last_register_number;
        // Set `last_register_number` for parents
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_register(last_register_number);
        }
    }

    fn inc_register(&mut self) {
        self.set_register(self.last_register_number + 1);
    }

    /// Set `inner_name` to current state and all parent states
    fn set_inner_name_and_to_parents(&mut self, name: &ValueName) {
        self.inner_values_name.insert(name.clone());
        if let Some(parent) = &self.parent {
            parent.borrow_mut().set_inner_name_and_to_parents(name);
        }
    }

    fn get_parent_value_name(&self, name: &ValueName) -> Option<Value> {
        if let Some(val) = self.values.get(name) {
            return Some(val.clone());
        } else if let Some(parent) = &self.parent {
            return parent.borrow().get_parent_value_name(name);
        }
        None
    }

    /// Get and set next label for any condition operations
    fn get_and_set_next_label(&mut self, label: &str) -> String {
        if !self.labels.contains(label) {
            self.labels.insert(label.to_string());
            return label.to_string();
        }
        let val_attr: Vec<&str> = label.split('.').collect();
        let name = if val_attr.len() == 2 {
            let i: u64 = val_attr[1].parse().expect("expect integer");
            format!("{}.{:?}", val_attr[0], i + 1)
        } else {
            format!("{}.0", val_attr[0])
        };
        if self.labels.contains(&name) {
            self.get_and_set_next_label(&name)
        } else {
            self.labels.insert(name.clone());
            name.to_string()
        }
    }

    fn get_next_inner_name(&self, val: &str) -> String {
        // Increment inner value name counter for shadowed variable
        let val_attr: Vec<&str> = val.split('.').collect();
        let name = if val_attr.len() == 2 {
            let i: u64 = val_attr[1].parse().expect("expect integer");
            format!("{}.{:?}", val_attr[0], i + 1)
        } else {
            format!("{}.0", val_attr[0])
        };
        if self.inner_values_name.contains(&name) {
            self.get_next_inner_name(&name)
        } else {
            name
        }
    }
}

#[derive(Debug)]
pub struct Function {
    pub inner_name: String,
    pub inner_type: InnerType,
    pub parameters: Vec<InnerType>,
}

#[derive(Debug)]
pub struct GlobalState {
    pub constants: HashMap<String, Constant>,
    pub types: HashSet<InnerType>,
    pub functions: HashMap<String, Function>,
}

#[derive(Debug)]
pub struct State<T: Codegen> {
    pub global: GlobalState,
    pub codegen: T,
}

#[derive(Debug)]
pub enum ExpressionResult {
    PrimitiveValue(PrimitiveValue),
    Register(u64),
}

impl<T: Codegen<Backend = T>> State<T> {
    pub fn new(codegen: T) -> Self {
        Self {
            global: GlobalState {
                functions: HashMap::new(),
                types: HashSet::new(),
                constants: HashMap::new(),
            },
            codegen,
        }
    }

    pub fn run(&mut self, data: &ast::Main<'_>) -> StateResults<()> {
        let result_errors = data.iter().fold(vec![], |mut s_err, main| {
            let res = match main {
                ast::MainStatement::Import(import) => self.import(import),
                ast::MainStatement::Constant(constant) => self.constant(constant),
                ast::MainStatement::Types(types) => self.types(types),
                ast::MainStatement::Function(function) => self.function(function),
            };
            if let Err(err) = res {
                s_err.push(err);
            }
            s_err
        });
        // After getting all functions declarations, fetch only functions body
        let result_errors = data.iter().fold(result_errors, |mut s_err, main| {
            let res = match main {
                ast::MainStatement::Function(function) => self.function_body(function),
                _ => return s_err,
            };
            if let Err(mut err) = res {
                s_err.append(&mut err);
            }
            s_err
        });
        if result_errors.is_empty() {
            Ok(())
        } else {
            Err(result_errors)
        }
    }

    #[allow(clippy::unused_self, clippy::unnecessary_wraps)]
    pub const fn import(&self, _data: &ast::ImportPath<'_>) -> StateResult<()> {
        Ok(())
    }

    pub fn types(&mut self, data: &ast::StructTypes<'_>) -> StateResult<()> {
        if self.global.types.contains(&data.name()) {
            return Err(error::StateErrorResult::new(
                error::StateErrorKind::TypeAlreadyExist,
                data.name(),
                0,
                0,
            ));
        }
        self.global.types.insert(data.name());
        self.codegen.types(data);
        Ok(())
    }

    pub fn constant(&mut self, data: &ast::Constant<'_>) -> StateResult<()> {
        if self.global.constants.contains_key(&data.name()) {
            return Err(error::StateErrorResult::new(
                error::StateErrorKind::ConstantAlreadyExist,
                data.name(),
                0,
                0,
            ));
        }
        self.global.constants.insert(
            data.name(),
            Constant {
                name: data.name(),
                inner_type: data.constant_type.name(),
            },
        );
        self.codegen.constant(data);
        Ok(())
    }

    pub fn function(&mut self, data: &ast::FunctionStatement<'_>) -> StateResult<()> {
        if self.global.functions.contains_key(&data.name()) {
            return Err(error::StateErrorResult::new(
                error::StateErrorKind::FunctionAlreadyExist,
                data.name(),
                0,
                0,
            ));
        }
        self.global.functions.insert(
            data.name(),
            Function {
                inner_name: data.name(),
                inner_type: data.result_type.name(),
                parameters: data
                    .parameters
                    .iter()
                    .map(|p| p.parameter_type.name())
                    .collect(),
            },
        );
        self.codegen.function_declaration(data);
        Ok(())
    }

    pub fn function_body(&mut self, data: &ast::FunctionStatement<'_>) -> StateResults<()> {
        let body_state = Rc::new(RefCell::new(ValueBlockState::new(None)));
        let mut return_is_called = false;
        let mut result_errors = data.body.iter().fold(vec![], |mut s_err, body| {
            let res = match body {
                ast::BodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &body_state).map_err(|e| vec![e])
                }
                ast::BodyStatement::FunctionCall(fn_call) => self
                    .function_call(fn_call, &body_state)
                    .map_err(|e| vec![e]),
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state)
                }
                ast::BodyStatement::Loop(loop_statement) => self
                    .loop_statement(loop_statement, &body_state)
                    .map_err(|e| vec![e]),
                ast::BodyStatement::Expression(expression) => {
                    let expr_result = self.expression(expression, &body_state);
                    expr_result
                        .map(|res| {
                            // TODO: Check is previously return was called
                            return_is_called = true;
                            self.codegen.expression_function_return(&res);
                        })
                        .map_err(|e| vec![e])
                }
                ast::BodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &body_state);
                    expr_result
                        .map(|res| {
                            // TODO: Check is previously return was called
                            return_is_called = true;
                            self.codegen.expression_function_return(&res);
                        })
                        .map_err(|e| vec![e])
                }
            };
            if let Err(mut err) = res {
                s_err.append(&mut err);
            }
            s_err
        });
        if !return_is_called {
            result_errors.push(error::StateErrorResult::new(
                error::StateErrorKind::ReturnNotFound,
                String::new(),
                0,
                0,
            ));
        }
        if result_errors.is_empty() {
            Ok(())
        } else {
            Err(result_errors)
        }
    }

    /// # Let-binding statement
    /// Analyze let-binding statement:
    /// 1. Let value bind from expression. First analyse should be
    /// `expression` for binding value.
    /// 2. Generate value for current state. Special field `inner_name`
    /// that used as name for codegen should be unique in current state and
    /// for all parent states. For that `inner_name` for value incremented.
    /// 3. Set for the value type and allocation status
    /// 4. Insert value to current block state
    /// 5. Store `inner_name` in current and parent states
    /// 6. Codegen
    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'_>,
        state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<()> {
        // Call value analytics before putting let-value to state
        // Put to the block state
        let expr_result = self.expression(&data.value, state)?;

        // Find value in current state and parent states
        let inner_value = state.borrow().get_parent_value_name(&data.name());
        // Calculate `inner_name` as unique for current and all parent states
        let inner_name = inner_value.map_or_else(
            || {
                // if value not found in all states
                data.name()
            },
            |val| {
                // Increment inner value name counter for shadowed variable
                // and check variable inner_name for and inner_values in current state
                state.borrow().get_next_inner_name(&val.inner_name)
            },
        );
        // Insert value to current block state
        let value = Value {
            inner_name: inner_name.clone(),
            inner_type: data
                .clone()
                .value_type
                // TODO: resolve type from expression for empty case
                .map_or(String::new(), |ty| ty.name()),
            allocated: false,
        };
        state.borrow_mut().values.insert(data.name(), value.clone());
        // Set `inner_name` to current state and all parent states
        state
            .borrow_mut()
            .set_inner_name_and_to_parents(&inner_name);

        self.codegen.let_binding(&value, &expr_result);
        Ok(())
    }

    /// # Function-call
    /// Call function with function parameters arguments. Arguments is
    /// expressions.
    /// 1. Check is current function name exists in global state of functions
    /// name.
    /// 2. Analyse expressions for function parameters
    /// 3. Generate codegen
    ///
    /// ## Errors
    /// Return error if function name doesn't exist in global state
    pub fn function_call(
        &mut self,
        data: &ast::FunctionCall<'_>,
        body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<()> {
        // Check is function exists in global functions state
        if !self.global.functions.contains_key(&data.name()) {
            return Err(error::StateErrorResult::new(
                error::StateErrorKind::FunctionNotFound,
                data.name(),
                0,
                0,
            ));
        }
        // Analyse function parameters expressions and set result to array
        let mut params: Vec<ExpressionResult> = vec![];
        for expr in &data.parameters {
            params.push(self.expression(expr, body_state)?);
        }

        // Codegen for function-call
        body_state.borrow_mut().inc_register();
        self.codegen
            .call(data, params, body_state.borrow().last_register_number);
        Ok(())
    }

    /// # condition-expression
    /// Analyse condition operations.    
    pub fn condition_expression(
        &mut self,
        data: &ast::ExpressionLogicCondition<'_>,
        function_body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResults<()> {
        // Analyse left expression of left condition
        let left_expr = &data.left.left;
        let left_res = self
            .expression(left_expr, function_body_state)
            .map_err(|err| vec![err])?;

        // Analyse right expression of left condition
        let right_expr = &data.left.right;
        let right_res = self
            .expression(right_expr, function_body_state)
            .map_err(|err| vec![err])?;

        // Increase register counter before generate condition
        function_body_state.borrow_mut().inc_register();
        // Codegen for condition
        self.codegen
            .condition_expression(&left_res, &right_res, &data.left.condition);

        if let Some(right) = &data.right {
            // Analyse right part of condition
            self.condition_expression(&right.1, function_body_state)?;
            // Increase register counter before generate logic condition
            function_body_state.borrow_mut().inc_register();
            // TODO: logic condition
        }

        Ok(())
    }

    /// # If-condition
    /// Includes all variants for if statements:
    /// 1. if
    /// 2. if-else
    /// 3. if-else-if
    /// It creates own state, with parent function-state. in that case
    /// if-state independent form parent state buy csn get access to
    /// parent state.
    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'_>,
        function_body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResults<()> {
        // Create state for if-body, from parent function state because if-state
        // can contain sub-state, that for can be independent from parent state
        let if_body_state = Rc::new(RefCell::new(ValueBlockState::new(Some(
            function_body_state.clone(),
        ))));
        let label_if_begin = if_body_state.borrow_mut().get_and_set_next_label(IF_BEGIN);
        let label_if_end = if_body_state.borrow_mut().get_and_set_next_label(IF_END);
        // Analyse if-conditions
        match &data.condition {
            ast::IfCondition::Single(expr) => {
                // Calculate expression for single if-condition expression
                let expr_result = self
                    .expression(expr, &if_body_state)
                    .map_err(|err| vec![err])?;
                // Codegen for if-condition from expression and if-body start
                self.codegen
                    .if_condition_expression(&expr_result, &label_if_begin, &label_if_end);
            }
            ast::IfCondition::Logic(expr_logic) => {
                // Analyse if-condition logic
                self.condition_expression(expr_logic, &if_body_state)?;
                // Codegen for if-condition-logic with if-body start
                self.codegen.if_condition_logic(
                    &label_if_begin,
                    &label_if_end,
                    if_body_state.borrow().last_register_number,
                );
            }
        }
        // Analyse if-statement body
        let result_errors = data.body.iter().fold(vec![], |mut s_err, body| {
            let res = match body {
                ast::IfBodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &if_body_state).map_err(|e| vec![e])
                }
                ast::IfBodyStatement::FunctionCall(fn_call) => self
                    .function_call(fn_call, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::IfBodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &if_body_state)
                }
                ast::IfBodyStatement::Loop(loop_statement) => self
                    .loop_statement(loop_statement, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::IfBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &if_body_state);
                    expr_result
                        .map(|res| {
                            // TODO: set return-is-called and pass it
                            // to main body
                            self.codegen.expression_function_return(&res);
                        })
                        .map_err(|e| vec![e])
                }
            };
            if let Err(mut err) = res {
                s_err.append(&mut err);
            }
            s_err
        });
        self.codegen.if_end(&label_if_end);

        // if-else gas own state, different from if-state
        let _if_else_body_state = Rc::new(RefCell::new(ValueBlockState::new(Some(
            function_body_state.clone(),
        ))));
        // Analyse if-else body: data.else_statement
        // TODO: if-else-body analyse
        // - update function-body (parent) state

        // Analyse all else-if statements
        if let Some(else_if_statements) = &data.else_if_statement {
            for else_if_statement in else_if_statements {
                // Analyse statement as independent
                let _res = self.if_condition(else_if_statement, function_body_state);
            }
        }

        if result_errors.is_empty() {
            Ok(())
        } else {
            Err(result_errors)
        }
    }

    #[allow(dead_code)]
    /// # IF-loop-statement
    /// If flow used only for loops. It's same as basic if-condition flow
    /// but also additional flow for: break, continue
    pub fn if_loop_condition(
        &mut self,
        data: &ast::IfLoopStatement<'_>,
        function_body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResults<()> {
        // Create state for if-body, from parent function state because if-state
        // can contain sub-state, that for can be independent from parent state
        let if_body_state = Rc::new(RefCell::new(ValueBlockState::new(Some(
            function_body_state.clone(),
        ))));
        // Analyse if-conditions
        match &data.condition {
            ast::IfCondition::Single(expr) => {
                // Calculate expression
                let _expr_result = self.expression(expr, function_body_state);
                // Codegen for if-condition from expression and if-body start
                // self.codegen.if_condition_expression(&expr_result, &function_body_state);
            }
            ast::IfCondition::Logic(_expr_logic) => {
                // 1. Call function for analyse if-condition logic
                // 2. Codegen for if-condition-logic with if-body start
            }
        }
        // Analyse if-statement body
        let result_errors = data.body.iter().fold(vec![], |mut s_err, body| {
            let res = match body {
                ast::IfLoopBodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &if_body_state).map_err(|e| vec![e])
                }
                ast::IfLoopBodyStatement::FunctionCall(fn_call) => self
                    .function_call(fn_call, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::IfLoopBodyStatement::IfLoop(if_condition) => {
                    self.if_loop_condition(if_condition, &if_body_state)
                }
                ast::IfLoopBodyStatement::Loop(loop_statement) => self
                    .loop_statement(loop_statement, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::IfLoopBodyStatement::Return(expression) => {
                    let expr_result = self.expression(expression, &if_body_state);
                    expr_result
                        .map(|res| {
                            // TODO: set return-is-called and pass it
                            // to main body
                            self.codegen.expression_function_return(&res);
                        })
                        .map_err(|e| vec![e])
                }
                ast::IfLoopBodyStatement::Break => {
                    todo!();
                }
                ast::IfLoopBodyStatement::Continue => {
                    todo!();
                }
            };
            if let Err(mut err) = res {
                s_err.append(&mut err);
            }
            s_err
        });
        // Update register for parent state
        function_body_state
            .borrow_mut()
            .set_register(if_body_state.borrow().last_register_number);

        // if-else gas own state, different from if-state
        let _if_else_body_state = Rc::new(RefCell::new(ValueBlockState::new(Some(
            function_body_state.clone(),
        ))));
        // Analyse if-else body: data.else_statement
        // TODO: if-else-body analyse
        // - update function-body (parent) state

        // Analyse all else-if statements
        if let Some(else_if_statements) = &data.else_if_statement {
            for else_if_statement in else_if_statements {
                // Analyse statement as independent
                let _res = self.if_loop_condition(else_if_statement, function_body_state);
            }
        }

        if result_errors.is_empty() {
            Ok(())
        } else {
            Err(result_errors)
        }
    }

    #[allow(clippy::unused_self, clippy::unnecessary_wraps)]
    pub const fn loop_statement(
        &self,
        _data: &[ast::LoopBodyStatement<'_>],
        _body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<()> {
        Ok(())
    }

    #[allow(clippy::doc_markdown)]
    /// ## Expression
    /// Is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    /// Return: PrimitiveValue | TmpRegister
    ///
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
        body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<ExpressionResult> {
        // To analyze expression first time, we set:
        // left_value - as None
        // operation - as None
        // And basic expression value is "right_value", because
        // it can contain sub-operations
        self.expression_operation(None, data, None, body_state)
    }

    /// Expression operation semantic logic:
    /// `OP(lhs, rhs)`
    pub fn expression_operation(
        &mut self,
        left_value: Option<&ExpressionResult>,
        right_expression: &ast::Expression<'_>,
        op: Option<&ExpressionOperations>,
        body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<ExpressionResult> {
        // Get right value from expression.
        // If expression return error immediately return error
        // because next analyzer should use success result/
        let right_value = match &right_expression.expression_value {
            ast::ExpressionValue::ValueName(value) => {
                let value_from_state = body_state.borrow_mut().get_parent_value_name(&value.name());
                if value_from_state.is_some() || self.global.constants.contains_key(&value.name()) {
                    // Increase register counter before loading value
                    body_state.borrow_mut().inc_register();
                    // First check value in body state
                    if let Some(val) = value_from_state {
                        // If it's value then Load it to register
                        self.codegen
                            .expression_value(&val, body_state.borrow().last_register_number);
                    } else if let Some(const_val) = self.global.constants.get(&value.name()) {
                        // If value is constant load it to register
                        self.codegen
                            .expression_const(const_val, body_state.borrow().last_register_number);
                    }
                    Ok(ExpressionResult::Register(
                        body_state.borrow().last_register_number,
                    ))
                } else {
                    // If value doesn't exist
                    Err(error::StateErrorResult::new(
                        error::StateErrorKind::ValueNotFound,
                        value.name(),
                        0,
                        0,
                    ))
                }
            }
            ast::ExpressionValue::PrimitiveValue(value) => {
                Ok(ExpressionResult::PrimitiveValue(value.clone()))
            }
            ast::ExpressionValue::FunctionCall(fn_call) => {
                body_state.borrow_mut().inc_register();
                let call_result = self.function_call(fn_call, body_state);
                call_result
                    .map(|_| ExpressionResult::Register(body_state.borrow().last_register_number))
            }
        }?;
        // It's special case for "pure" expression - without operation.
        // For that also left side of expression shouldn't exist
        if left_value.is_none() || op.is_none() {
            return Ok(right_value);
        }
        // Call expression operation for: OP(left_value, right_value)
        // and return result of that call as register
        body_state.borrow_mut().inc_register();
        self.codegen.expression_operation(
            op.unwrap(),
            left_value.unwrap(),
            &right_value,
            body_state.borrow().last_register_number,
        );
        let expression_result =
            ExpressionResult::Register(body_state.borrow().last_register_number);

        // Check is for right value exist next operation
        if let Some((operation, expr)) = &right_expression.operation {
            self.expression_operation(Some(&expression_result), expr, Some(operation), body_state)
        } else {
            Ok(expression_result)
        }
    }
}

mod error {
    #[derive(Debug, Clone)]
    pub enum StateErrorKind {
        ConstantAlreadyExist,
        TypeAlreadyExist,
        FunctionAlreadyExist,
        ValueNotFound,
        FunctionNotFound,
        ReturnNotFound,
    }

    #[derive(Debug, Clone)]
    pub struct StateErrorLocation {
        line: u64,
        column: u64,
    }

    #[derive(Debug, Clone)]
    pub struct StateErrorResult {
        kind: StateErrorKind,
        value: String,
        location: StateErrorLocation,
    }

    impl StateErrorResult {
        pub const fn new(kind: StateErrorKind, value: String, line: u64, column: u64) -> Self {
            Self {
                kind,
                value,
                location: StateErrorLocation { line, column },
            }
        }
    }

    impl StateErrorResult {
        pub fn trace_state(&self) -> String {
            format!(
                "[{:?}] for value {:?} at: {:?}:{:?}",
                self.kind, self.value, self.location.line, self.location.column
            )
        }
    }
}
