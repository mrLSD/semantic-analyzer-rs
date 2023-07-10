#![deny(clippy::pedantic, clippy::nursery)]
use crate::backend::dummy::Backend;
use crate::semantic::State;

mod ast;
mod backend;
mod codegen;
mod semantic;
mod types;

fn main() {
    let source: ast::Main = vec![];
    let _res = State::new(Backend::new()).run(&source);
    println!("#> ...");
    // if let Err(err) = res {
    //     for trace in &err {
    //         println!("{}", trace.trace_state());
    //     }
    // }
}
