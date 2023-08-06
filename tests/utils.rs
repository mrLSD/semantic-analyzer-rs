use semantic_analyzer::codegen::CodegenStack;
use semantic_analyzer::semantic::State;
use semantic_analyzer::types::error::StateErrorKind;

pub struct SemanticTest {
    pub state: State<CodegenStack>,
}

impl Default for SemanticTest {
    fn default() -> Self {
        Self::new()
    }
}

impl SemanticTest {
    pub fn new() -> Self {
        let backend = CodegenStack::new();
        Self {
            state: State::new(backend),
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
