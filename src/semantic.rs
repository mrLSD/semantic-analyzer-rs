use crate::ast::{self, ExpressionOperations, GetName, PrimitiveValue};
use crate::codegen::Codegen;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

type ValueName = String;
type InnerType = String;

#[derive(Debug)]
pub struct Constant {
    pub name: String,
    pub inner_type: InnerType,
}

#[derive(Debug)]
pub struct Value {
    pub inner_name: ValueName,
    pub inner_type: InnerType,
    pub allocated: bool,
}

#[derive(Debug)]
pub struct ValueBlockState {
    pub values: HashMap<ValueName, Value>,
    pub last_register_number: u64,
    pub parent: Option<Rc<RefCell<ValueBlockState>>>,
}

impl ValueBlockState {
    fn new(parent: Option<Rc<RefCell<Self>>>) -> Self {
        Self {
            values: HashMap::new(),
            last_register_number: 0,
            parent,
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
            return Err(StateErrorResult::new(
                StateErrorKind::TypeAlreadyExist,
                data.name(),
                0,
                0,
            ));
        }
        self.global.types.insert(data.name());
        self.codegen = self.codegen.types(data);
        Ok(())
    }

    pub fn constant(&mut self, data: &ast::Constant<'_>) -> StateResult<()> {
        if self.global.constants.contains_key(&data.name()) {
            return Err(StateErrorResult::new(
                StateErrorKind::ConstantAlreadyExist,
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
        self.codegen = self.codegen.constant(data);
        Ok(())
    }

    pub fn function(&mut self, data: &ast::FunctionStatement<'_>) -> StateResult<()> {
        if self.global.functions.contains_key(&data.name()) {
            return Err(StateErrorResult::new(
                StateErrorKind::FunctionAlreadyExist,
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
        self.codegen = self.codegen.function_declaration(data);
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
                            return_is_called = true;
                            self.codegen = self.codegen.expression_function_return(&res);
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
            result_errors.push(StateErrorResult::new(
                StateErrorKind::ReturnNotFound,
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

    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'_>,
        state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<()> {
        // Call value analytics before putting let-value to state
        // Put to the block state
        let expr_result = self.expression(&data.value, state)?;
        let inner_name = state.borrow_mut().values.get(&data.name()).map_or_else(
            || data.name(),
            |val| {
                if val.allocated {
                    // TODO: deallocate
                }
                // Increment inner value name counter for shadowed variable
                let val_attr: Vec<&str> = val.inner_name.split('.').collect();
                if val_attr.len() == 2 {
                    let i: u64 = val_attr[1].parse().expect("expect integer");
                    format!("{}.{:?}", val_attr[0], i + 1)
                } else {
                    format!("{}.0", val_attr[0])
                }
            },
        );
        state.borrow_mut().values.insert(
            data.name(),
            Value {
                inner_name,
                inner_type: data
                    .clone()
                    .value_type
                    // TODO: resolve type from expression for empty case
                    .map_or(String::new(), |ty| ty.name()),
                allocated: false,
            },
        );

        self.codegen = self.codegen.let_binding(data, &expr_result);
        Ok(())
    }

    pub fn function_call(
        &mut self,
        data: &ast::FunctionCall<'_>,
        body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResult<()> {
        if !self.global.functions.contains_key(&data.name()) {
            return Err(StateErrorResult::new(
                StateErrorKind::FunctionNotFound,
                data.name(),
                0,
                0,
            ));
        }
        body_state.borrow_mut().last_register_number += 1;
        self.codegen = self
            .codegen
            .call(data, body_state.borrow().last_register_number);
        Ok(())
    }

    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'_>,
        function_body_state: &Rc<RefCell<ValueBlockState>>,
    ) -> StateResults<()> {
        let if_body_state = Rc::new(RefCell::new(ValueBlockState::new(Some(
            function_body_state.clone(),
        ))));
        let result_errors = data.body.iter().fold(vec![], |mut s_err, body| {
            let res = match body {
                ast::BodyStatement::LetBinding(bind) => {
                    self.let_binding(bind, &if_body_state).map_err(|e| vec![e])
                }
                ast::BodyStatement::FunctionCall(fn_call) => self
                    .function_call(fn_call, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &if_body_state)
                }
                ast::BodyStatement::Loop(loop_statement) => self
                    .loop_statement(loop_statement, &if_body_state)
                    .map_err(|e| vec![e]),
                ast::BodyStatement::Expression(expression) => {
                    let expr_result = self.expression(expression, &if_body_state);
                    expr_result
                        .map(|res| {
                            self.codegen = self.codegen.expression_function_return(&res);
                        })
                        .map_err(|e| vec![e])
                }
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
    pub const fn loop_statement(
        &self,
        _data: &[ast::BodyStatement<'_>],
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
        // And basic expression value is "right_value", becuase
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
                if body_state.borrow_mut().values.contains_key(&value.name())
                    || self.global.constants.contains_key(&value.name())
                {
                    // Increase register counter before loading value
                    body_state.borrow_mut().last_register_number += 1;
                    // First check value in body state
                    if let Some(val) = body_state.borrow().values.get(&value.name()) {
                        // If it's value then Load it to register
                        self.codegen = self
                            .codegen
                            .expression_value(val, body_state.borrow().last_register_number);
                    } else if let Some(const_val) = self.global.constants.get(&value.name()) {
                        // If value is constant load it to register
                        self.codegen = self
                            .codegen
                            .expression_const(const_val, body_state.borrow().last_register_number);
                    }
                    Ok(ExpressionResult::Register(
                        body_state.borrow().last_register_number,
                    ))
                } else {
                    // If value doesn't exist
                    Err(StateErrorResult::new(
                        StateErrorKind::ValueNotFound,
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
                body_state.borrow_mut().last_register_number += 1;
                let call_result = self.function_call(fn_call, body_state);
                call_result
                    .map(|_| ExpressionResult::Register(body_state.borrow().last_register_number))
            }
        }?;
        // It's special case for "pure" expression - without operation.
        // For that also left side of expression shouldn't exist
        if left_value.is_none() || op.is_none() {
            return Ok(ExpressionResult::Register(
                body_state.borrow().last_register_number,
            ));
        }
        // Call expression operation for: OP(left_value, right_value)
        // and return result of that call as register
        body_state.borrow_mut().last_register_number += 1;
        self.codegen = self.codegen.expression_operation(
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
    const fn new(kind: StateErrorKind, value: String, line: u64, column: u64) -> Self {
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

pub type StateResult<T> = Result<T, StateErrorResult>;
pub type StateResults<T> = Result<T, Vec<StateErrorResult>>;
