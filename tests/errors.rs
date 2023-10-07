use semantic_analyzer::ast::CodeLocation;
use semantic_analyzer::types::error::{StateErrorKind, StateErrorLocation, StateErrorResult};

#[test]
fn error_trace() {
    let err_res = StateErrorResult::new(
        StateErrorKind::Common,
        "test".to_string(),
        CodeLocation::new(1, 1),
    );
    assert_eq!(err_res.value, "test".to_string());
    assert_eq!(err_res.kind, StateErrorKind::Common);
    assert_eq!(err_res.trace_state(), "[Common] for value \"test\" at: 1:1");

    let err_res2 = StateErrorResult {
        kind: StateErrorKind::ReturnNotFound,
        value: "test2".to_string(),
        location: StateErrorLocation(CodeLocation::new(2, 2)),
    };
    assert_eq!(
        err_res2.trace_state(),
        "[ReturnNotFound] for value \"test2\" at: 2:2"
    );
    let errs = vec![err_res, err_res2.clone()];
    assert_eq!(errs.len(), 2);
}
