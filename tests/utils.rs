use semantic_analyzer::semantic::State;
use semantic_analyzer::types::error::StateErrorKind;

pub struct SemanticTest {
    pub state: State,
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
        self.state.errors.get(0).unwrap().kind == err_kind
    }
}
