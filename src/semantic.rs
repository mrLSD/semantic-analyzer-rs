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
        let mut body_state = BodyState::new();
        data.body
            .iter()
            .map(|body| match body {
                ast::BodyStatement::LetBinding(bind) => self.let_binding(bind, &mut body_state),
                ast::BodyStatement::FunctionCall(_) => StateResult,
                ast::BodyStatement::If(_) => StateResult,
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
}
