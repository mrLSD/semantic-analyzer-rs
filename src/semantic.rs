#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(clippy::unused_self)]
#![allow(clippy::ptr_arg)]

use crate::ast;
use crate::ast::GetName;
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
    pub parent: Option<Rc<ValueBlockState>>,
}

impl ValueBlockState {
    fn new(parent: Option<Rc<Self>>) -> Self {
        Self {
            values: HashMap::new(),
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

impl<T: Codegen> State<T> {
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
        // let body_state = BodyState::new();
        // self.body_statement(&data.body, body_state)
        vec![]
    }

    pub fn function_body(&mut self, data: &ast::FunctionStatement<'_>) -> Vec<StateResult> {
        let _body_state: ValueBlockState = ValueBlockState::new(None);
        /*
        let mut body_state = body_state;
        data.iter().fold(vec![], |mut s, body| {
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
        })
        */
        vec![]
    }

    #[allow(clippy::unused_self)]
    pub fn let_binding(
        &self,
        data: &ast::LetBinding<'_>,
        state: &mut ValueBlockState,
    ) -> Vec<StateResult> {
        //state.values.insert(data.name(), data.clone());
        vec![]
    }

    /// Function call do not change state analyzer but use it
    pub fn function_call(
        &mut self,
        data: &ast::FunctionCall<'_>,
        body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        /*
        let res = if body_state.functions.contains_key(&data.name())
            || self.global.functions.contains_key(&data.name())
        {
            vec![]
        } else {
            vec![StateResult::FunctionNotFound]
        };
        data.parameters.iter().fold(res, |mut s, e| {
            let mut res_expr = self.expression(e, body_state);
            s.append(&mut res_expr);
            s
        })
            */
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

    /// Expression is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    pub fn expression(
        &mut self,
        data: &ast::Expression<'_>,
        body_state: &ValueBlockState,
    ) -> Vec<StateResult> {
        /*
        let mut res = match &data.expression_value {
            ast::ExpressionValue::ValueName(value) => {
                // First check value in body state
                if body_state.values.contains_key(&value.name())
                    || self.global.constants.contains_key(&value.name())
                {
                    vec![]
                } else if self.global.imports.contains_key(&value.name()) {
                    todo!("implement import modules analyzer for external constants")
                } else {
                    vec![StateResult::ValueNotFound]
                }
            }
            // do nothing for primitive value
            ast::ExpressionValue::PrimitiveValue(_value) => vec![],
            ast::ExpressionValue::FunctionCall(fn_call) => self.function_call(fn_call, body_state),
        };
        if let Some(e) = &data.operation {
            let mut res_mut = self.expression(&e.1, body_state);
            res.append(&mut res_mut);
        }
        res
        */
        vec![]
    }
}
