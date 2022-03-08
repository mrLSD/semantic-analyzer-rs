#![deny(clippy::pedantic, clippy::nursery)]
use crate::backend::dummy::Backend;
use crate::semantic::State;

mod ast;
mod backend;
mod codegen;
mod semantic;

fn main() {
    let source: ast::Main = vec![];
    let res = State::new(Backend::new()).run(&source);
    println!("#> ... {res:?}");
}
