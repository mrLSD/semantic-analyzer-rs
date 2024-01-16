use crate::utils::SemanticTest;
use semantic_analyzer::ast::{self, GetName};

mod utils;

#[test]
fn import_ast_transform() {
    let import_name_ast = ast::ImportName::new(ast::Ident::new("import1"));
    let imports = [import_name_ast.clone()];
    assert_eq!(imports.len(), 1);
    assert_eq!(import_name_ast.name(), "import1");
    let x: ast::ImportPath = vec![import_name_ast];
    assert_eq!(x.len(), 1);
    let t = SemanticTest::new();
    t.state.import(&x);
}
