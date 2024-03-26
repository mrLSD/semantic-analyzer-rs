use semantic_analyzer::semantic::State;
use semantic_analyzer::types::block_state::BlockState;
use semantic_analyzer::types::error::StateErrorKind;
use semantic_analyzer::types::expression::{ExpressionResult, ExpressionResultValue};
use semantic_analyzer::types::semantic::{ExtendedExpression, SemanticContextInstruction};
use semantic_analyzer::types::types::{PrimitiveTypes, Type};
use semantic_analyzer::types::PrimitiveValue;
#[cfg(feature = "codec")]
use serde::{Deserialize, Serialize};
use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct CustomExpression<I: SemanticContextInstruction> {
    _marker: PhantomData<I>,
}

impl<I: SemanticContextInstruction> ExtendedExpression<I> for CustomExpression<I> {
    fn expression(
        &self,
        _state: &mut State<Self, I>,
        _block_state: &Rc<RefCell<BlockState<I>>>,
    ) -> ExpressionResult {
        ExpressionResult {
            expr_type: Type::Primitive(PrimitiveTypes::Ptr),
            expr_value: ExpressionResultValue::PrimitiveValue(PrimitiveValue::Ptr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "codec", derive(Serialize, Deserialize))]
pub struct CustomExpressionInstruction;

impl SemanticContextInstruction for CustomExpressionInstruction {}

pub struct SemanticTest<I: SemanticContextInstruction> {
    pub state: State<CustomExpression<I>, I>,
}

impl Default for SemanticTest<CustomExpressionInstruction> {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticTest<CustomExpressionInstruction> {
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
