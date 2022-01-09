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
    pub values: Vec<ast::LetBinding<'a>>,
    pub functions: Vec<ast::FunctionStatement<'a>>,
}

#[derive(Debug)]
pub struct State<'a> {
    pub global: GlobalState<'a>,
    pub body: BodyState<'a>,
}

impl<'a> State<'a> {
    pub fn main(&mut self, data: &ast::MainStatement<'a>) {
        match data {
            ast::MainStatement::Import(import) => self.import(import),
            ast::MainStatement::Constant(constant) => self.constant(constant),
            ast::MainStatement::Function(_) => (),
        }
    }

    pub fn import(&mut self, data: &ast::ImportPath<'a>) {
        self.global.imports.insert(data[0].name(), data.to_owned());
    }

    pub fn constant(&mut self, data: &ast::Constant<'a>) {
        self.global.constants.insert(data.name(), data.clone());
    }
}
