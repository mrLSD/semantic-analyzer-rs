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
    pub fn check_error(&self, err_kind: StateErrorKind) -> bool {
        if self.state.errors.len() != 1 {
            return false;
        }
        if let Some(err) = self.state.errors.get(0) {
            err.kind == err_kind
        } else {
            false
        }
    }
}
