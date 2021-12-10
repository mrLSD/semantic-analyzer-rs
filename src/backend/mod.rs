#![allow(dead_code)]
use crate::ast;
use crate::ast::GetName;
use crate::codegen::Codegen;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, PointerValue},
};
use std::collections::HashMap;

//mod ink;

pub struct Compiler<'a, 'ctx> {
    pub context: &'ctx Context,
    pub module: &'a Module<'ctx>,
    pub builder: &'a Builder<'ctx>,
    pub fpm: &'a PassManager<FunctionValue<'ctx>>,

    variables: HashMap<String, PointerValue<'ctx>>,
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'a, 'ctx> Compiler<'a, 'ctx> {
    pub fn new(
        context: &'ctx Context,
        module: &'a Module<'ctx>,
        builder: &'a Builder<'ctx>,
        fpm: &'a PassManager<FunctionValue<'ctx>>,
    ) -> Self {
        Self {
            context,
            module,
            builder,
            fpm,
            variables: HashMap::new(),
            fn_value_opt: None,
        }
    }

    pub fn compile() {
        let context = Context::create();
        let module = context.create_module("main");
        let builder = context.create_builder();

        // Create FPM
        let fpm = PassManager::create(&module);

        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();
        fpm.add_gvn_pass();
        fpm.add_cfg_simplification_pass();
        fpm.add_basic_alias_analysis_pass();
        fpm.add_promote_memory_to_register_pass();
        fpm.add_instruction_combining_pass();
        fpm.add_reassociate_pass();

        fpm.initialize();
        let _compiler = Compiler::new(&context, &module, &builder, &fpm);
    }
}

impl<'a, 'ctx> Codegen for Compiler<'a, 'ctx> {
    fn function_declaration(&self, fn_decl: ast::FunctionStatement) {
        let param_types = fn_decl
            .parameters
            .iter()
            .map(|param| match &param.parameter_type {
                ast::Type::Primitive(ty) => match ty {
                    ast::PrimitiveTypes::I8 => self.context.i8_type().into(),
                    _ => self.context.i8_type().into(),
                },
                _ => self.context.i64_type().into(),
            })
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type = self.context.f64_type().fn_type(&param_types, false);
        let _fn_val = self.module.add_function(&fn_decl.name(), fn_type, None);
    }

    fn expression(&self, _expression: ast::Expression) -> &Self {
        self
    }
}
