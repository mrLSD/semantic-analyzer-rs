#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(clippy::unused_self)]
#![allow(clippy::ptr_arg)]

use crate::ast;
use crate::ast::{GetName, PrimitiveTypes};
use crate::codegen::Codegen;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;
use std::str::FromStr;

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
    Success,
    ConstantAlreadyExist,
    TypeAlreadyExist,
    FunctionAlreadyExist,
    ValueNotFound,
    TypeNotFound,
    FunctionNotFound,
}

pub enum ExpressionResult<T: FromStr> {
    PrimitiveValue(PrimitiveTypes, T),
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
        // After gathe all functions declarations, fetch only functions body
        data.iter().fold(res, |mut s, main| {
            let mut res = match main {
                ast::MainStatement::Function(function) => self.function_body(function),
                _ => return s,
            };
            s.append(&mut res);
            s
        })
    }

    pub fn import(&mut self, _data: &ast::ImportPath<'_>) -> Vec<StateResult> {
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
                    self.function_call(fn_call, &body_state)
                }
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state)
                }
                ast::BodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, &body_state)
                }
                ast::BodyStatement::Expression(expression) => {
                    self.expression(expression, &body_state)
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
        //      - al3loca, store
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
        _state: &ValueBlockState,
    ) -> Vec<StateResult> {
        if !self.global.functions.contains_key(&data.name()) {
            return vec![StateResult::FunctionNotFound];
        }
        self.codegen = self.codegen.call(data);
        vec![]
    }

    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'_>,
        body_state: &ValueBlockState,
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

    pub fn loop_statement(
        &mut self,
        data: &Vec<ast::BodyStatement<'_>>,
        body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        // self.body_statement(data, body_state)
        vec![]
    }

    #[allow(clippy::doc_markdown)]
    /// Expression is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    /// Return: PrimitiveValue | TmpRegister
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
        body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        let res = match &data.expression_value {
            ast::ExpressionValue::ValueName(value) => {
                // First check value in body state
                if body_state.values.contains_key(&value.name())
                    || self.global.constants.contains_key(&value.name())
                {
                    vec![]
                } else {
                    vec![StateResult::ValueNotFound]
                }
                // TODO: load-instruction
            }
            ast::ExpressionValue::PrimitiveValue(_value) => {
                // TODO: store-instruction: store i32 x, 2
                vec![]
            }
            ast::ExpressionValue::FunctionCall(fn_call) => self.function_call(fn_call, body_state),
        };
        /*
        if let Some(e) = &data.operation {
            let mut res_mut = self.expression(&e.1, body_state);
            res.append(&mut res_mut);
        }
        res
        */
        vec![]
    }
}
