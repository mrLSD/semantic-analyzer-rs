#![allow(dead_code)]

use crate::codegen::Codegen;
use inkwell::types::{ArrayType, BasicType, BasicTypeEnum, FloatType, IntType, StructType};
// use inkwell::values::BasicValue;
use crate::types::{
    Condition, Constant, ExpressionOperations, ExpressionResult, Function, FunctionStatement,
    LabelName, LogicCondition, PrimitiveTypes, StructTypes, Type, Value,
};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    passes::PassManager,
    values::{FunctionValue, PointerValue},
};
use std::collections::HashMap;

pub mod dummy;
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

    pub fn get_type<T>(&self, ty: &Type) -> T
    where
        T: From<IntType<'ctx>>
            + From<FloatType<'ctx>>
            + From<ArrayType<'ctx>>
            + From<StructType<'ctx>>,
    {
        match ty {
            Type::Primitive(ty) => match ty {
                PrimitiveTypes::I8 | PrimitiveTypes::U8 | PrimitiveTypes::Char => {
                    self.context.i8_type().into()
                }
                PrimitiveTypes::I16 | PrimitiveTypes::U16 => self.context.i16_type().into(),
                PrimitiveTypes::I32 | PrimitiveTypes::U32 => self.context.i32_type().into(),
                PrimitiveTypes::I64 | PrimitiveTypes::U64 => self.context.i64_type().into(),
                PrimitiveTypes::F32 => self.context.f32_type().into(),
                PrimitiveTypes::F64 => self.context.f64_type().into(),
                PrimitiveTypes::Bool => self.context.bool_type().into(),
                PrimitiveTypes::String | PrimitiveTypes::Ptr | PrimitiveTypes::None => todo!(),
            },
            Type::Struct(ty_struct) => {
                let struct_types = ty_struct
                    .types
                    .iter()
                    .map(|ty| self.get_type(&ty.attr_type))
                    .collect::<Vec<BasicTypeEnum>>();
                self.context.struct_type(&struct_types[..], false).into()
            }
            Type::Array(ty, capacity) => {
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
    fn function_declaration(&mut self, _fn_decl: &FunctionStatement) {
        todo!()
    }

    fn constant(&mut self, _const_decl: &Constant) {
        todo!()
    }

    fn types(&mut self, _type_decl: &StructTypes) {
        todo!()
    }

    fn function_statement(&mut self, _fn_decl: &FunctionStatement) {
        todo!()
    }

    fn let_binding(&mut self, _let_decl: &Value, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn binding(&mut self, _val: &Value, _expr_result: &ExpressionResult) {
        todo!()
    }

    fn call(&mut self, _call: &Function, _params: Vec<ExpressionResult>, _register_number: u64) {
        todo!()
    }

    fn expression_value(&mut self, _expression: &Value, _register_number: u64) {
        todo!()
    }

    fn expression_struct_value(&mut self, _expression: &Value, _index: u64, _register_number: u64) {
        todo!()
    }

    fn expression_const(&mut self, _expression: &Constant, _register_number: u64) {
        todo!()
    }

    fn expression_operation(
        &mut self,
        _operation: &ExpressionOperations,
        _left_value: &ExpressionResult,
        _right_value: &ExpressionResult,
        _register_number: u64,
    ) {
        todo!()
    }

    fn expression_function_return(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn jump_function_return(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn expression_function_return_with_label(&mut self, _expr_result: &ExpressionResult) {
        todo!()
    }
    fn if_condition_expression(
        &mut self,
        _expr_result: &ExpressionResult,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
    ) {
        todo!();
    }
    fn if_condition_logic(
        &mut self,
        _label_if_begin: &LabelName,
        _label_if_end: &LabelName,
        _register_number: u64,
    ) {
        todo!();
    }
    fn jump_to(&mut self, _label: &LabelName) {
        todo!();
    }
    fn set_label(&mut self, _label: &LabelName) {
        todo!()
    }
    fn condition_expression(
        &mut self,
        _left_result: &ExpressionResult,
        _right_result: &ExpressionResult,
        _condition: &Condition,
        _register_number: u64,
    ) {
        todo!();
    }

    fn logic_condition(
        &mut self,
        _left_condition_register: u64,
        _right_condition_register: u64,
        _logic_condition: &LogicCondition,
        _register_number: u64,
    ) {
        todo!()
    }
}
