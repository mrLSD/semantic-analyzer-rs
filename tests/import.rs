use semantic_analyzer::ast::{self, GetName};

#[test]
fn import_ast_transform() {
    let import_name_ast = ast::ImportName::new(ast::Ident::new("import1"));
    assert_eq!(import_name_ast.name(), "import1");
}
