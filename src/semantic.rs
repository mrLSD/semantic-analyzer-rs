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

#[derive(Debug)]
pub struct BodyState<'a> {
    pub values: HashMap<String, ast::LetBinding<'a>>,
    pub functions: HashMap<String, ast::FunctionStatement<'a>>,
}

#[derive(Debug)]
pub struct State<'a> {
    pub global: GlobalState<'a>,
}

pub struct StateResult;

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
        data.iter().fold(vec![], |mut s: Vec<StateResult>, main| {
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
        data.iter()
            .map(|body| match body {
                ast::BodyStatement::LetBinding(bind) => self.let_binding(bind, &mut body_state),
                ast::BodyStatement::FunctionCall(fn_call) => {
                    self.function_call(fn_call, &body_state)
                }
                ast::BodyStatement::If(if_condition) => {
                    self.if_condition(if_condition, &body_state)
                }
                ast::BodyStatement::Loop(_) => StateResult,
                ast::BodyStatement::Expression(_) => StateResult,
            })
            .collect()
    }

    pub fn let_binding(
        &mut self,
        data: &ast::LetBinding<'a>,
        state: &mut BodyState<'a>,
    ) -> StateResult {
        state.values.insert(data.name(), data.clone());
        StateResult
    }

    /// Function call do not change state
    pub fn function_call(
        &mut self,
        _data: &ast::FunctionCall<'a>,
        _state: &BodyState<'a>,
    ) -> StateResult {
        StateResult
    }

    pub fn if_condition(
        &mut self,
        _data: &ast::IfStatement<'a>,
        state: &BodyState<'a>,
    ) -> StateResult {
        let _state = state;
        StateResult
    }
}
