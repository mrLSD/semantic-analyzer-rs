use crate::ast::{self, ExpressionOperations, GetName, PrimitiveValue};
use crate::codegen::Codegen;
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
    pub parent: Option<Rc<ValueBlockState>>,
}

impl ValueBlockState {
    fn new(parent: Option<Rc<Self>>) -> Self {
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

#[derive(Debug, Clone)]
pub enum StateResult {
    ConstantAlreadyExist,
    TypeAlreadyExist,
    FunctionAlreadyExist,
    ValueNotFound,
    FunctionNotFound,
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

    pub fn run(&mut self, data: &ast::Main<'_>) -> Vec<StateResult> {
        let res = data.iter().fold(vec![], |mut s, main| {
            let mut res = match main {
                ast::MainStatement::Import(import) => self.import(import),
                ast::MainStatement::Constant(constant) => self.constant(constant),
                ast::MainStatement::Types(types) => self.types(types),
                ast::MainStatement::Function(function) => self.function(function),
            };
            s.append(&mut res);
            s
        });
        // After getting all functions declarations, fetch only functions body
        data.iter().fold(res, |mut s, main| {
            let mut res = match main {
                ast::MainStatement::Function(function) => self.function_body(function),
                _ => return s,
            };
            s.append(&mut res);
            s
        })
    }

    #[allow(clippy::unused_self)]
    pub const fn import(&self, _data: &ast::ImportPath<'_>) -> Vec<StateResult> {
        // TODO: process imports
        vec![]
    }

    pub fn types(&mut self, data: &ast::StructTypes<'_>) -> Vec<StateResult> {
        if self.global.types.contains(&data.name()) {
            return vec![StateResult::TypeAlreadyExist];
        }
        self.global.types.insert(data.name());
        self.codegen = self.codegen.types(data);
        vec![]
    }

    pub fn constant(&mut self, data: &ast::Constant<'_>) -> Vec<StateResult> {
        if self.global.constants.contains_key(&data.name()) {
            return vec![StateResult::ConstantAlreadyExist];
        }
        self.global.constants.insert(
            data.name(),
            Constant {
                name: data.name(),
                inner_type: data.constant_type.name(),
            },
        );
        self.codegen = self.codegen.constant(data);
        vec![]
    }

    pub fn function(&mut self, data: &ast::FunctionStatement<'_>) -> Vec<StateResult> {
        if self.global.functions.contains_key(&data.name()) {
            return vec![StateResult::FunctionAlreadyExist];
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
        vec![]
    }

    pub fn function_body(&mut self, data: &ast::FunctionStatement<'_>) -> Vec<StateResult> {
        let mut body_state: ValueBlockState = ValueBlockState::new(None);
        data.body.iter().fold(vec![], |mut s, body| {
            let mut res = match body {
                ast::BodyStatement::LetBinding(bind) => self.let_binding(bind, &mut body_state),
                ast::BodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, &mut body_state)
                }
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state)
                }
                ast::BodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &body_state)
                }
                ast::BodyStatement::Expression(expression) => {
                    let res = self.expression(expression, &mut body_state);
                    res.1
                }
            };
            s.append(&mut res);
            s
        });
        vec![]
    }

    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'_>,
        state: &mut ValueBlockState,
    ) -> Vec<StateResult> {
        let inner_name = state.values.get(&data.name()).map_or_else(
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
        state.values.insert(
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
        // TODO: fetch let-binding.value
        // 1. Body::Function
        //   - alloca, call, store
        // 2. Body::let - deprecated as nonsense
        // 3. Body::Expression
        //   3.1. Expr::PrimitiveValue
        //      - alloca, store
        //   3.2. Expr::FuncCall
        //      - alloca, call, store
        //   3.3. Expr::Value
        //      - alloca, load, store
        //   3.4. Expr::PrimitiveValue - OP - Expr::PrimitiveValue
        //      - alloca, tmp = OP val1 val2, store tmp
        //   3.5. Expr::Value - OP - Expr::PrimitiveValue
        //      - alloca, tmp1 = load, tmp2 = OP tmp2, val1, store
        //   3.6. Expr::FuncCall - OP - Expr::PrimitiveValue
        //      - alloca, tmp1 = call, tmp2 = OP tmp2, val1, store
        //   3.7. Expr::PrimitiveValue - OP - Expr::Value
        //      - Same as 3.4
        //   3.8. Expr::Value - OP - Expr::Value
        //      - alloca, tmp1 = load, tmp2 = load, tmp3 = OP tmp1 tmp2, store tmp3
        //   3.9. Expr::FuncCall - OP - Expr::Value
        //      - alloca, tmp1 = call, tmp2 = Load, tmp3 = OP tmp1 tmp2, store tmp3
        //   3.10. Expr::FuncCall - OP - Expr::FuncCall
        //      - alloca, tmp1 = call, tmp2 = call, tmp3 = OP tmp1 tmp2, store tmp3
        self.codegen = self.codegen.let_binding(data);
        vec![]
    }

    pub fn function_call(
        &mut self,
        data: &ast::FunctionCall<'_>,
        body_state: &mut ValueBlockState,
    ) -> Vec<StateResult> {
        if !self.global.functions.contains_key(&data.name()) {
            return vec![StateResult::FunctionNotFound];
        }
        body_state.last_register_number += 1;
        self.codegen = self.codegen.call(data, body_state.last_register_number);
        vec![]
    }

    #[allow(clippy::unused_self)]
    pub const fn if_condition(
        &self,
        _data: &ast::IfStatement<'_>,
        _body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        /*
        let mut res = self.body_statement(&data.body, body_state.clone());
        if let Some(data) = &data.else_statement {
            let mut r = self.body_statement(data, body_state.clone());
            res.append(&mut r);
        }
        if let Some(data) = &data.else_if_statement {
            let mut r = data.iter().fold(vec![], |mut r, if_stmt| {
                let mut res = self.if_condition(if_stmt, body_state);
                r.append(&mut res);
                r
            });
            res.append(&mut r);
        }
        res
            */
        vec![]
    }

    #[allow(clippy::unused_self)]
    pub const fn loop_statement(
        &self,
        _data: &[ast::BodyStatement<'_>],
        _body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        // self.body_statement(data, body_state)
        vec![]
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
        body_state: &mut ValueBlockState,
    ) -> (Option<ExpressionResult>, Vec<StateResult>) {
        let res = match &data.expression_value {
            ast::ExpressionValue::ValueName(value) => {
                // First check value in body state
                if let Some(val) = body_state.values.get(&value.name()) {
                    body_state.last_register_number += 1;
                    self.codegen = self
                        .codegen
                        .expression_value(val, body_state.last_register_number);
                    (
                        Some(ExpressionResult::Register(body_state.last_register_number)),
                        vec![],
                    )
                } else if let Some(const_val) = self.global.constants.get(&value.name()) {
                    body_state.last_register_number += 1;
                    self.codegen = self
                        .codegen
                        .expression_const(const_val, body_state.last_register_number);
                    (
                        Some(ExpressionResult::Register(body_state.last_register_number)),
                        vec![],
                    )
                } else {
                    (None, vec![StateResult::ValueNotFound])
                }
            }
            ast::ExpressionValue::PrimitiveValue(value) => (
                Some(ExpressionResult::PrimitiveValue(value.clone())),
                vec![],
            ),
            ast::ExpressionValue::FunctionCall(fn_call) => {
                let res = self.function_call(fn_call, body_state);
                (
                    Some(ExpressionResult::Register(body_state.last_register_number)),
                    res,
                )
            }
        };
        if res.0.is_none() {
            return res;
        }

        if let Some(expr) = &data.operation {
            let mut state_res = res.1;
            let expr_op = self.expression_operation(&res.0.unwrap(), &expr.1, &expr.0, body_state);
            let mut expr_op_state_res = expr_op.1;
            state_res.append(&mut expr_op_state_res);
            if expr_op.0.is_none() {
                return (None, state_res);
            }
            (expr_op.0, state_res)
        } else {
            res
        }
    }

    /// Expression operation:
    /// `OP(lhs, rhs)`
    pub fn expression_operation(
        &mut self,
        lhs: &ExpressionResult,
        rhs: &ast::Expression<'_>,
        op: &ExpressionOperations,
        body_state: &mut ValueBlockState,
    ) -> (Option<ExpressionResult>, Vec<StateResult>) {
        let op_result = match &rhs.expression_value {
            ast::ExpressionValue::ValueName(value) => {
                // First check value in body state
                if let Some(val) = body_state.values.get(&value.name()) {
                    body_state.last_register_number += 1;
                    let left_value = lhs;
                    let right_value = ExpressionResult::Register(body_state.last_register_number);
                    self.codegen = self
                        .codegen
                        .expression_value(val, body_state.last_register_number);
                    self.codegen = self.codegen.expression_operation(
                        op,
                        left_value,
                        &right_value,
                        body_state.last_register_number,
                    );
                    (
                        Some(ExpressionResult::Register(body_state.last_register_number)),
                        vec![],
                    )
                } else if let Some(const_val) = self.global.constants.get(&value.name()) {
                    body_state.last_register_number += 1;
                    let left_value = lhs;
                    let right_value = ExpressionResult::Register(body_state.last_register_number);
                    self.codegen = self
                        .codegen
                        .expression_const(const_val, body_state.last_register_number);
                    self.codegen = self.codegen.expression_operation(
                        op,
                        left_value,
                        &right_value,
                        body_state.last_register_number,
                    );
                    (
                        Some(ExpressionResult::Register(body_state.last_register_number)),
                        vec![],
                    )
                } else {
                    (None, vec![StateResult::ValueNotFound])
                }
            }
            ast::ExpressionValue::PrimitiveValue(value) => {
                body_state.last_register_number += 1;
                let left_value = lhs;
                let right_value = ExpressionResult::PrimitiveValue(value.clone());
                self.codegen = self.codegen.expression_operation(
                    op,
                    left_value,
                    &right_value,
                    body_state.last_register_number,
                );
                (
                    Some(ExpressionResult::Register(body_state.last_register_number)),
                    vec![],
                )
            }
            ast::ExpressionValue::FunctionCall(fn_call) => {
                body_state.last_register_number += 1;
                let left_value = lhs;
                let right_value = ExpressionResult::Register(body_state.last_register_number);
                let call_result = self.function_call(fn_call, body_state);
                self.codegen = self.codegen.expression_operation(
                    op,
                    left_value,
                    &right_value,
                    body_state.last_register_number,
                );
                (
                    Some(ExpressionResult::Register(body_state.last_register_number)),
                    call_result,
                )
            }
        };
        if op_result.0.is_none() {
            return op_result;
        }
        if let Some(expr) = &rhs.operation {
            let mut state_res = op_result.1;
            let expr_op =
                self.expression_operation(&op_result.0.unwrap(), &expr.1, &expr.0, body_state);
            let mut expr_op_state_res = expr_op.1;
            state_res.append(&mut expr_op_state_res);
            if expr_op.0.is_none() {
                return (None, state_res);
            }
            (expr_op.0, state_res)
        } else {
            op_result
        }
    }
}
