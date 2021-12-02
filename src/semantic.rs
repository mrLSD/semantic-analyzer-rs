#![allow(dead_code)]
#![allow(clippy::ptr_arg)]

use crate::ast::{self, GetName};
use std::collections::HashMap;

#[derive(Debug)]
pub struct GlobalState<'a> {
    pub imports: HashMap<String, ast::ImportPath<'a>>,
    pub constants: HashMap<String, ast::Constant<'a>>,
    pub functions: HashMap<String, ast::FunctionStatement<'a>>,
}

#[derive(Debug, Clone)]
pub struct BodyState<'a> {
    pub values: HashMap<String, ast::LetBinding<'a>>,
    pub functions: HashMap<String, ast::FunctionStatement<'a>>,
}

#[derive(Debug)]
pub struct State<'a> {
    pub global: GlobalState<'a>,
}

#[derive(Debug, Clone)]
pub enum StateResult {
    ValueNotFound,
}

impl<'a> BodyState<'a> {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

impl<'a> State<'a> {
    pub fn new() -> Self {
        Self {
            global: GlobalState {
                functions: HashMap::new(),
                imports: HashMap::new(),
                constants: HashMap::new(),
            },
        }
    }

    pub fn main(&mut self, data: &ast::Main<'a>) -> Vec<StateResult> {
        data.iter().fold(vec![], |mut s, main| {
            let mut res = match main {
                ast::MainStatement::Import(import) => self.import(import),
                ast::MainStatement::Constant(constant) => self.constant(constant),
                ast::MainStatement::Function(function) => self.function(function),
            };
            s.append(&mut res);
            s
        })
    }

    /// Set import module (las element in import path)
    pub fn import(&mut self, data: &ast::ImportPath<'a>) -> Vec<StateResult> {
        self.global
            .imports
            .insert(data[data.len() - 1].name(), data.to_owned());
        vec![]
    }

    pub fn constant(&mut self, data: &ast::Constant<'a>) -> Vec<StateResult> {
        self.global.constants.insert(data.name(), data.clone());
        vec![]
    }

    pub fn function(&mut self, data: &ast::FunctionStatement<'a>) -> Vec<StateResult> {
        self.global.functions.insert(data.name(), data.clone());
        // Init Body state. It's root state
        let body_state = BodyState::new();
        self.body_statement(&data.body, body_state)
    }

    pub fn body_statement(
        &mut self,
        data: &Vec<ast::BodyStatement<'a>>,
        body_state: BodyState<'a>,
    ) -> Vec<StateResult> {
        let mut body_state = body_state;
        data.iter().fold(vec![], |mut s, body| {
            let mut res = match body {
                ast::BodyStatement::LetBinding(bind) => self.let_binding(bind, &mut body_state),
                ast::BodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, &body_state)
                }

                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, body_state.clone())
                }
                ast::BodyStatement::Loop(loop_statement) => {
                    self.loop_statement(loop_statement, body_state.clone())
                }
                ast::BodyStatement::Expression(expression) => {
                    self.expression(expression, body_state.clone())
                }
            };
            s.append(&mut res);
            s
        })
    }

    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'a>,
        state: &mut BodyState<'a>,
    ) -> Vec<StateResult> {
        state.values.insert(data.name(), data.clone());
        vec![]
    }

    /// Function call do not change state
    pub fn function_call(
        &mut self,
        _data: &ast::FunctionCall<'a>,
        _state: &BodyState<'a>,
    ) -> Vec<StateResult> {
        vec![]
    }

    pub fn if_condition(
        &mut self,
        data: &ast::IfStatement<'a>,
        body_state: BodyState<'a>,
    ) -> Vec<StateResult> {
        let mut res = self.body_statement(&data.body, body_state.clone());
        if let Some(data) = &data.else_statement {
            let mut r = self.body_statement(data, body_state.clone());
            res.append(&mut r)
        }
        if let Some(data) = &data.else_if_statement {
            let mut r = data.iter().fold(vec![], |mut r, if_stmt| {
                let mut res = self.if_condition(if_stmt, body_state.clone());
                r.append(&mut res);
                r
            });
            res.append(&mut r)
        }
        res
    }

    pub fn loop_statement(
        &mut self,
        data: &Vec<ast::BodyStatement<'a>>,
        body_state: BodyState<'a>,
    ) -> Vec<StateResult> {
        self.body_statement(data, body_state.clone())
    }

    /// Expression is basic entity for state operation and state usage.
    /// State correctness verified by expressions call.
    pub fn expression(
        &mut self,
        data: &ast::Expression<'a>,
        body_state: BodyState<'a>,
    ) -> Vec<StateResult> {
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
            ast::ExpressionValue::FunctionCall(fn_call) => self.function_call(fn_call, &body_state),
        };
        if let Some(e) = &data.operation {
            let mut res_mut = self.expression(&e.1, body_state);
            res.append(&mut res_mut);
        }
        res
    }
}
