#![deny(clippy::pedantic, clippy::nursery)]
use crate::backend::dummy::Backend;
use crate::semantic::State;

mod ast;
mod backend;
mod codegen;
mod semantic;

fn main() {
    State::new(Backend::new());
    println!("#> ...");
}
