use crate::ast;

#[derive(Debug)]
pub struct GlobalState<'a> {
    pub imports: Vec<ast::ImportPath<'a>>,
    pub constants: Vec<ast::Constant<'a>>,
    pub functions: Vec<ast::FunctionStatement<'a>>,
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

pub trait BindGenerator<'a> {
    fn bind_gen(&self, state: &'a State);
}

impl<'a> BindGenerator<'a> for ast::Main<'a> {
    fn bind_gen(&self, state: &'a State) {
        let _: Vec<()> = self.iter().map(|main| main.bind_gen(state)).collect();
    }
}

impl<'a> BindGenerator<'a> for ast::MainStatement<'a> {
    fn bind_gen(&self, _state: &'a State) {
        match self {
            ast::MainStatement::Import(_) => (),
            ast::MainStatement::Constant(_) => (),
            ast::MainStatement::Function(_) => (),
        }
    }
}
