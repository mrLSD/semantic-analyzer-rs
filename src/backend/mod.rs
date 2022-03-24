#![allow(dead_code)]

pub mod dummy;

use crate::ast::{Constant, GetName, StructTypes};
use crate::codegen::Codegen;
use crate::{ast, semantic};
use inkwell::types::{
    ArrayType, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FloatType, IntType, StructType,
};
// use inkwell::values::BasicValue;
use crate::semantic::{ExpressionResult, Value};
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

    pub fn get_type<T>(&self, ty: &ast::Type) -> T
    where
        T: From<IntType<'ctx>>
            + From<FloatType<'ctx>>
            + From<ArrayType<'ctx>>
            + From<StructType<'ctx>>,
    {
        match ty {
            ast::Type::Primitive(ty) => match ty {
                ast::PrimitiveTypes::I8 | ast::PrimitiveTypes::U8 | ast::PrimitiveTypes::Char => {
                    self.context.i8_type().into()
                }
                ast::PrimitiveTypes::I16 | ast::PrimitiveTypes::U16 => {
                    self.context.i16_type().into()
                }
                ast::PrimitiveTypes::I32 | ast::PrimitiveTypes::U32 => {
                    self.context.i32_type().into()
                }
                ast::PrimitiveTypes::I64 | ast::PrimitiveTypes::U64 => {
                    self.context.i64_type().into()
                }
                ast::PrimitiveTypes::F32 => self.context.f32_type().into(),
                ast::PrimitiveTypes::F64 => self.context.f64_type().into(),
                ast::PrimitiveTypes::Bool => self.context.bool_type().into(),
                ast::PrimitiveTypes::String | ast::PrimitiveTypes::None => todo!(),
            },
            ast::Type::Struct(ty_struct) => {
                let struct_types = ty_struct
                    .types
                    .iter()
                    .map(|ty| self.get_type(&ty.attr_type))
                    .collect::<Vec<BasicTypeEnum>>();
                self.context.struct_type(&struct_types[..], false).into()
            }
            ast::Type::Array(ty, capacity) => {
                let ty_array: BasicTypeEnum = self.get_type(ty);
                ty_array.array_type(*capacity).into()
            }
        }
    }

    /// Creates a new stack allocation instruction in the
    /// entry block of the function.
    fn create_entry_block_alloca(
        &self,
        fn_value: FunctionValue<'ctx>,
        name: &str,
    ) -> PointerValue<'ctx> {
        let builder = self.context.create_builder();
        let entry = fn_value.get_first_basic_block().unwrap();
        entry.get_first_instruction().map_or_else(
            || builder.position_at_end(entry),
            |first_instr| builder.position_before(&first_instr),
        );
        builder.build_alloca(self.context.f64_type(), name)
    }
}

impl<'a, 'ctx> Codegen for Compiler<'a, 'ctx> {
    type Backend = FunctionValue<'ctx>;
    fn function_declaration(&self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend {
        let param_types = fn_decl
            .parameters
            .iter()
            .map(|param| self.get_type(&param.parameter_type))
            .collect::<Vec<BasicMetadataTypeEnum>>();
        let fn_type = self.context.f64_type().fn_type(&param_types, false);
        self.module.add_function(&fn_decl.name(), fn_type, None)
    }

    fn constant(&self, _const_decl: &Constant<'_>) -> Self::Backend {
        todo!()
    }

    fn types(&self, _type_decl: &StructTypes<'_>) -> Self::Backend {
        todo!()
    }

    fn function_statement(&mut self, fn_decl: &ast::FunctionStatement<'_>) -> Self::Backend {
        let fn_value = self.function_declaration(fn_decl);

        // Set functions parameters value name
        for (i, arg) in fn_value.get_param_iter().enumerate() {
            arg.set_name(fn_decl.parameters[i].name().as_str());
        }

        // Check empty body
        if fn_decl.body.is_empty() {
            return fn_value;
        }

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        // Allocate variables
        for (i, arg) in fn_value.get_param_iter().enumerate() {
            let arg_name = fn_decl.parameters[i].name();
            let alloca = self.create_entry_block_alloca(fn_value, arg_name.as_str());
            self.builder.build_store(alloca, arg);
            self.variables.insert(arg_name, alloca);
        }

        if fn_value.verify(true) {
            self.fpm.run_on(&fn_value);
            fn_value
        } else {
            unsafe {
                fn_value.delete();
            }
            panic!("Wrong function");
        }
    }

    fn let_binding(&self, _let_decl: &Value, _expr_result: &ExpressionResult) -> Self::Backend {
        todo!()
    }

    fn call(
        &self,
        _call: &ast::FunctionCall<'_>,
        _params: Vec<ExpressionResult>,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }

    fn expression_value(
        &self,
        _expression: &semantic::Value,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }

    fn expression_const(
        &self,
        _expression: &semantic::Constant,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }

    fn expression_operation(
        &self,
        _operation: &ast::ExpressionOperations,
        _left_value: &semantic::ExpressionResult,
        _right_value: &semantic::ExpressionResult,
        _register_number: u64,
    ) -> Self::Backend {
        todo!()
    }

    fn expression_function_return(&self, _expr_result: &ExpressionResult) -> Self::Backend {
        todo!()
    }
}
