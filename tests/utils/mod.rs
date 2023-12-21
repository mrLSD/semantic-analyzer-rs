use semantic_analyzer::semantic::State;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::ExpressionResult;
use semantic_analyzer::types::semantic::{ExtendedExpression, GetAst};
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct CustomExpression;

impl GetAst for CustomExpression {
    type Ast = ();

    fn get_ast(&self) -> Self::Ast {
        todo!()
    }
}

impl ExtendedExpression for CustomExpression {
    fn expression(
        &self,
        _state: &mut State<Self>,
        _block_state: &Rc<RefCell<BlockState>>,
    ) -> ExpressionResult {
        todo!()
    }
}

pub struct SemanticTest {
    pub state: State<CustomExpression>,
}

impl Default for SemanticTest {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticTest {
    pub fn new() -> Self {
        Self {
            state: State::new(),
        }
    }

    #[allow(dead_code)]
    pub fn is_empty_error(&self) -> bool {
        self.state.errors.is_empty()
    }

    #[allow(dead_code)]
    pub fn clean_errors(&mut self) {
        self.state.errors = vec![]
    }

    #[allow(dead_code)]
    pub fn check_errors_len(&self, len: usize) -> bool {
        self.state.errors.len() == len
    }

    #[allow(dead_code)]
    pub fn check_error(&self, err_kind: StateErrorKind) -> bool {
        self.state.errors.first().unwrap().kind == err_kind
    }

    #[allow(dead_code)]
    pub fn check_error_index(&self, index: usize, err_kind: StateErrorKind) -> bool {
        self.state.errors.get(index).unwrap().kind == err_kind
    }
}
