use semantic_analyzer::ast;
use semantic_analyzer::ast::{ConstantName, Ident};

#[test]
fn main_transform() {
    let imports: ast::ImportPath = vec![ast::ImportName::new(Ident::new("import1"))];
    let import_stm = ast::MainStatement::Import(imports);
    let constant1 = ast::Constant {
        name: ast::ConstantName::new(Ident::new("const1")),
        constant_type: ast::Type::Primitive(ast::PrimitiveTypes::None),
        constant_value: ast::ConstantExpression {
            value: ast::ConstantValue::Constant(ConstantName::new(Ident::new("const2"))),
            operation: None,
        },
    };
    let constant_stm = ast::MainStatement::Constant(constant1);
	let fn1 = ast::FunctionStatement{
		name:ast::FunctionName::new(Ident::new("fn1"));
	};
	let fn_stm = ast::MainStatement::Function(fn1);
    let _main_stm: ast::Main = vec![import_stm.clone(), constant_stm.clone()];
}
