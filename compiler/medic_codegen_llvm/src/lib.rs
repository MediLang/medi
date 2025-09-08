//! LLVM backend integration for Medi compiler (feature-gated).
//!
//! This crate provides a thin wrapper around LLVM via Inkwell and a feature-gated
//! AST-to-IR lowerer. When the `llvm` feature is enabled, we:
//! - Host a per-session `LLVMContext`
//! - Create/manage a `Module` and `IRBuilder`
//! - Translate a subset of Medi AST into LLVM IR (literals, identifiers, basic
//!   arithmetic and comparisons, let/assign, blocks, if/while, returns, and simple
//!   function definitions)
//! - Initialize targets (x86_64, WebAssembly, RISC-V)
//!
//! To enable, build with feature `llvm` and ensure LLVM 15.x is installed on your system.
//! Example:
//!   cargo build -p medic_codegen_llvm --features llvm

use std::marker::PhantomData;
use thiserror::Error;

#[cfg(feature = "llvm")]
use inkwell::types::BasicType;
#[cfg(feature = "llvm")]
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::StructType,
    types::{ArrayType, BasicMetadataTypeEnum, BasicTypeEnum, FloatType, IntType, VoidType},
    values::{
        BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
    },
    AddressSpace,
}; // for array_type()

#[cfg(feature = "llvm")]
use medic_ast::ast::*;

#[derive(Debug, Error)]
pub enum CodeGenError {
    #[error("LLVM feature not enabled: rebuild with `--features llvm`")]
    FeatureDisabled,
    #[cfg(feature = "llvm")]
    #[error("LLVM error: {0}")]
    Llvm(String),
}

/// Abstraction for a codegen context.
pub struct CodegenContext {
    #[cfg(feature = "llvm")]
    context: inkwell::context::Context,
}

impl CodegenContext {
    /// Create a new backend context.
    pub fn new() -> Result<Self, CodeGenError> {
        #[cfg(feature = "llvm")]
        {
            Ok(Self {
                context: inkwell::context::Context::create(),
            })
        }

        #[cfg(not(feature = "llvm"))]
        {
            Err(CodeGenError::FeatureDisabled)
        }
    }
}

/// Handle to a module where functions and globals are emitted.
pub struct ModuleHandle<'ctx> {
    #[cfg(feature = "llvm")]
    #[allow(dead_code)]
    module: inkwell::module::Module<'ctx>,
    // Ensure 'ctx is used even when the feature is disabled
    _phantom: PhantomData<&'ctx ()>,
}

/// Instruction builder wrapper.
pub struct IRBuilder<'ctx> {
    #[cfg(feature = "llvm")]
    #[allow(dead_code)]
    builder: inkwell::builder::Builder<'ctx>,
    // Ensure 'ctx is used even when the feature is disabled
    _phantom: PhantomData<&'ctx ()>,
}

/// Create a new module under the given context.
pub fn create_module<'ctx>(
    ctx: &'ctx CodegenContext,
    name: &str,
) -> Result<ModuleHandle<'ctx>, CodeGenError> {
    #[cfg(feature = "llvm")]
    {
        let module = ctx.context.create_module(name);
        Ok(ModuleHandle {
            module,
            _phantom: PhantomData,
        })
    }
    #[cfg(not(feature = "llvm"))]
    {
        let _ = (ctx, name);
        Err(CodeGenError::FeatureDisabled)
    }
}

/// Create a new IR builder.
pub fn create_builder<'ctx>(ctx: &'ctx CodegenContext) -> Result<IRBuilder<'ctx>, CodeGenError> {
    #[cfg(feature = "llvm")]
    {
        let builder = ctx.context.create_builder();
        Ok(IRBuilder {
            builder,
            _phantom: PhantomData,
        })
    }
    #[cfg(not(feature = "llvm"))]
    {
        let _ = ctx;
        Err(CodeGenError::FeatureDisabled)
    }
}

/// Initialize all codegen targets we care about.
/// Safe to call multiple times.
pub fn initialize_targets() -> Result<(), CodeGenError> {
    #[cfg(feature = "llvm")]
    {
        use inkwell::targets::Target;
        // Initialize all targets supported by linked LLVM.
        Target::initialize_all(&Default::default());
        Ok(())
    }

    #[cfg(not(feature = "llvm"))]
    {
        Err(CodeGenError::FeatureDisabled)
    }
}

// Intentionally no Pipeline struct owning and borrowing from the same context to avoid
// self-referential lifetime issues. Create builder/module as needed with an explicit
// scope where the borrow lives shorter than the context value.

/// Compilation targets supported by the backend.
#[derive(Debug, Clone, Copy)]
pub enum TargetKind {
    X86_64,
    Wasm32,
    RiscV32,
}

/// High-level entry: translate Medi AST into an LLVM module.
/// When the `llvm` feature is enabled, use `generate_ir_string(program)` instead
/// to lower a parsed `ProgramNode` into a Module and return the IR text.
pub fn generate_llvm_ir(_ast_json: &str) -> Result<(), CodeGenError> {
    initialize_targets()?;
    let _ = _ast_json;
    #[cfg(feature = "llvm")]
    {
        // Kept for API compatibility; prefer generate_ir_string().
        Ok(())
    }
    #[cfg(not(feature = "llvm"))]
    {
        Err(CodeGenError::FeatureDisabled)
    }
}

/// High-level optimizer entry for a module.
pub fn optimize_module(_level: u8) -> Result<(), CodeGenError> {
    // Placeholder: wire standard LLVM pass manager here in future change.
    initialize_targets()?;
    Ok(())
}

/// Emit target code for a given target kind.
pub fn generate_target_code(_target: TargetKind) -> Result<Vec<u8>, CodeGenError> {
    // Placeholder: will configure target machine and emit object/wasm.
    initialize_targets()?;
    Ok(Vec::new())
}

// ---------- AST -> IR translation (feature-gated) ----------

#[cfg(feature = "llvm")]
#[derive(Clone)]
struct StructInfo {
    type_name: String,
    #[allow(dead_code)]
    field_names: Vec<String>,
}

#[cfg(feature = "llvm")]
#[allow(clippy::type_complexity)]
struct Scope<'ctx> {
    /// Name -> (alloca pointer, element type)
    vars: Vec<
        std::collections::HashMap<
            String,
            (PointerValue<'ctx>, BasicTypeEnum<'ctx>, Option<StructInfo>),
        >,
    >,
}

#[cfg(feature = "llvm")]
impl<'ctx> Scope<'ctx> {
    fn new() -> Self {
        Self {
            vars: vec![Default::default()],
        }
    }
    fn push(&mut self) {
        self.vars.push(Default::default());
    }
    fn pop(&mut self) {
        self.vars.pop();
    }
    fn insert(&mut self, name: &str, ptr: PointerValue<'ctx>, elem_ty: BasicTypeEnum<'ctx>) {
        if let Some(top) = self.vars.last_mut() {
            top.insert(name.to_string(), (ptr, elem_ty, None));
        }
    }
    fn get(
        &self,
        name: &str,
    ) -> Option<(PointerValue<'ctx>, BasicTypeEnum<'ctx>, Option<StructInfo>)> {
        for m in self.vars.iter().rev() {
            if let Some(p) = m.get(name) {
                return Some(p.clone());
            }
        }
        None
    }
    fn insert_with_struct(
        &mut self,
        name: &str,
        ptr: PointerValue<'ctx>,
        elem_ty: BasicTypeEnum<'ctx>,
        info: StructInfo,
    ) {
        if let Some(top) = self.vars.last_mut() {
            top.insert(name.to_string(), (ptr, elem_ty, Some(info)));
        }
    }
}

#[cfg(feature = "llvm")]
struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    i64: IntType<'ctx>,
    f64: FloatType<'ctx>,
    i1: IntType<'ctx>,
    void: VoidType<'ctx>,
    scope: Scope<'ctx>,
    current_fn: Option<FunctionValue<'ctx>>,
    struct_types: std::collections::HashMap<String, (StructType<'ctx>, Vec<String>)>,
    // Minimal variant tag registry: variant name -> tag id
    variant_tags: std::collections::HashMap<String, i32>,
}

#[cfg(feature = "llvm")]
impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();
        let i64 = context.i64_type();
        let f64 = context.f64_type();
        let i1 = context.bool_type();
        let void = context.void_type();
        Self {
            context,
            module,
            builder,
            i64,
            f64,
            i1,
            void,
            scope: Scope::new(),
            current_fn: None,
            struct_types: std::collections::HashMap::new(),
            variant_tags: std::collections::HashMap::new(),
        }
    }

    fn get_variant_tag(&mut self, name: &str) -> i32 {
        if let Some(v) = self.variant_tags.get(name) {
            *v
        } else {
            // Assign incremental tags based on current size
            let tag = self.variant_tags.len() as i32;
            self.variant_tags.insert(name.to_string(), tag);
            tag
        }
    }

    fn type_from_annotation(&self, expr: &ExpressionNode) -> Option<BasicTypeEnum<'ctx>> {
        match expr {
            ExpressionNode::Identifier(id) => {
                let name = id.node.name().to_ascii_lowercase();
                match name.as_str() {
                    "int" | "i64" => Some(self.i64.into()),
                    "float" | "f64" => Some(self.f64.into()),
                    "bool" | "i1" => Some(self.i1.into()),
                    "string" | "str" | "i8*" => Some(
                        self.context
                            .i8_type()
                            .ptr_type(AddressSpace::from(0))
                            .into(),
                    ),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn basic_type_of_value(v: &BasicValueEnum<'ctx>) -> BasicTypeEnum<'ctx> {
        match v {
            BasicValueEnum::IntValue(iv) => iv.get_type().into(),
            BasicValueEnum::FloatValue(fv) => fv.get_type().into(),
            BasicValueEnum::PointerValue(pv) => pv.get_type().into(),
            BasicValueEnum::ArrayValue(av) => av.get_type().into(),
            BasicValueEnum::StructValue(sv) => sv.get_type().into(),
            BasicValueEnum::VectorValue(vv) => vv.get_type().into(),
        }
    }

    fn get_or_define_struct(
        &mut self,
        name: &str,
        field_types: &[BasicTypeEnum<'ctx>],
        field_names: &[String],
    ) -> Result<StructType<'ctx>, CodeGenError> {
        if let Some((st, existing_names)) = self.struct_types.get(name) {
            if existing_names == field_names && st.get_field_types() == field_types {
                return Ok(*st);
            }
            return Err(CodeGenError::Llvm(format!(
                "struct '{name}': conflicting definition (names/types differ)"
            )));
        }
        let st = self.context.opaque_struct_type(name);
        st.set_body(field_types, false);
        self.struct_types
            .insert(name.to_string(), (st, field_names.to_vec()));
        Ok(st)
    }

    fn store_struct_fields(
        &self,
        st: StructType<'ctx>,
        dest_ptr: PointerValue<'ctx>,
        field_vals: &[BasicValueEnum<'ctx>],
    ) -> Result<(), CodeGenError> {
        for (i, val) in field_vals.iter().enumerate() {
            let fld_ptr = self
                .builder
                .build_struct_gep(st, dest_ptr, i as u32, &format!("field.{i}"))
                .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
            self.builder.build_store(fld_ptr, *val);
        }
        Ok(())
    }

    fn infer_array_type(
        &self,
        vals: &[BasicValueEnum<'ctx>],
    ) -> Result<ArrayType<'ctx>, CodeGenError> {
        if vals.is_empty() {
            return Err(CodeGenError::Llvm(
                "cannot infer type for empty array literal".into(),
            ));
        }
        let first_ty = Self::basic_type_of_value(&vals[0]);
        for v in vals.iter().skip(1) {
            let ty = Self::basic_type_of_value(v);
            if ty != first_ty {
                return Err(CodeGenError::Llvm(
                    "array literal elements must have the same type".into(),
                ));
            }
        }
        Ok(first_ty.array_type(vals.len() as u32))
    }

    fn store_array_elements(
        &self,
        arr_ty: ArrayType<'ctx>,
        dest_ptr: PointerValue<'ctx>,
        vals: &[BasicValueEnum<'ctx>],
    ) -> Result<(), CodeGenError> {
        for (i, v) in vals.iter().enumerate() {
            let idx0 = self.i64.const_int(0, false);
            let idxi = self.i64.const_int(i as u64, false);
            let elem_ptr = unsafe {
                self.builder.build_in_bounds_gep(
                    arr_ty,
                    dest_ptr,
                    &[idx0, idxi],
                    &format!("elt.{i}"),
                )
            };
            self.builder.build_store(elem_ptr, *v);
        }
        Ok(())
    }

    fn basic_type_for_literal(&self, lit: &LiteralNode) -> BasicTypeEnum<'ctx> {
        match lit {
            LiteralNode::Int(_) => self.i64.into(),
            LiteralNode::Float(_) => self.f64.into(),
            LiteralNode::Bool(_) => self.i1.into(),
            LiteralNode::String(_) => {
                // For now, lower strings as i8* (opaque pointer for simplicity)
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::from(0))
                    .into()
            }
        }
    }

    fn const_for_literal(&self, lit: &LiteralNode) -> BasicValueEnum<'ctx> {
        match lit {
            LiteralNode::Int(i) => self.i64.const_int(*i as u64, true).into(),
            LiteralNode::Float(fl) => self.f64.const_float(*fl).into(),
            LiteralNode::Bool(b) => self.i1.const_int(if *b { 1 } else { 0 }, false).into(),
            LiteralNode::String(s) => {
                let gv = self.builder.build_global_string_ptr(s, "str");
                gv.as_pointer_value().into()
            }
        }
    }

    /// Convert an arbitrary computed value to an i1 boolean for conditions.
    fn to_bool(&self, v: BasicValueEnum<'ctx>) -> Result<IntValue<'ctx>, CodeGenError> {
        match v {
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() == 1 {
                    Ok(iv)
                } else {
                    Ok(self.builder.build_int_compare(
                        inkwell::IntPredicate::NE,
                        iv,
                        iv.get_type().const_zero(),
                        "tobool",
                    ))
                }
            }
            BasicValueEnum::FloatValue(fv) => Ok(self.builder.build_float_compare(
                inkwell::FloatPredicate::ONE,
                fv,
                fv.get_type().const_zero(),
                "toboolf",
            )),
            _ => Err(CodeGenError::Llvm(
                "unsupported truthiness for condition".into(),
            )),
        }
    }

    /// Short-circuit lowering for logical And/Or producing an i1 value.
    fn codegen_short_circuit(
        &mut self,
        be: &BinaryExpressionNode,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let func = self
            .current_fn
            .ok_or_else(|| CodeGenError::Llvm("logical op outside function".into()))?;
        let lhs_val = self.codegen_expr(&be.left)?;
        let lhs_bool = self.to_bool(lhs_val)?;
        let lhs_bb = self.builder.get_insert_block().expect("has block");

        let rhs_bb = self.context.append_basic_block(func, "logic.rhs");
        let cont_bb = self.context.append_basic_block(func, "logic.end");

        match be.operator {
            BinaryOperator::And => {
                // If lhs is true, evaluate rhs; else jump to cont with false
                self.builder
                    .build_conditional_branch(lhs_bool, rhs_bb, cont_bb);
                // rhs path
                self.builder.position_at_end(rhs_bb);
                let rhs_val = self.codegen_expr(&be.right)?;
                let rhs_bool = self.to_bool(rhs_val)?;
                let rhs_bb_end = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(cont_bb);
                // cont + phi
                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.i1, "and.tmp");
                let false_val: BasicValueEnum = self.i1.const_int(0, false).into();
                let rhs_bv: BasicValueEnum = rhs_bool.into();
                phi.add_incoming(&[(&rhs_bv, rhs_bb_end), (&false_val, lhs_bb)]);
                Ok(phi.as_basic_value())
            }
            BinaryOperator::Or => {
                // If lhs is true, jump to cont with true; else evaluate rhs
                self.builder
                    .build_conditional_branch(lhs_bool, cont_bb, rhs_bb);
                // rhs path
                self.builder.position_at_end(rhs_bb);
                let rhs_val = self.codegen_expr(&be.right)?;
                let rhs_bool = self.to_bool(rhs_val)?;
                let rhs_bb_end = self.builder.get_insert_block().unwrap();
                self.builder.build_unconditional_branch(cont_bb);
                // cont + phi
                self.builder.position_at_end(cont_bb);
                let phi = self.builder.build_phi(self.i1, "or.tmp");
                let true_val: BasicValueEnum = self.i1.const_int(1, false).into();
                let rhs_bv: BasicValueEnum = rhs_bool.into();
                phi.add_incoming(&[(&true_val, lhs_bb), (&rhs_bv, rhs_bb_end)]);
                Ok(phi.as_basic_value())
            }
            _ => unreachable!("only And/Or supported here"),
        }
    }

    fn create_entry_alloca(&self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        let function = self.current_fn.expect("alloca outside of function");
        let entry = function.get_first_basic_block().expect("fn without entry");
        // Use a temporary builder at the entry to place allocas before first instruction
        let tmp_builder = self.context.create_builder();
        if let Some(first_instr) = entry.get_first_instruction() {
            tmp_builder.position_before(&first_instr);
        } else {
            tmp_builder.position_at_end(entry);
        }
        tmp_builder.build_alloca(ty, name)
    }

    fn codegen_expr(
        &mut self,
        expr: &ExpressionNode,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        match expr {
            ExpressionNode::Literal(sp) => Ok(self.const_for_literal(&sp.node)),
            ExpressionNode::Identifier(sp) => {
                let name = sp.node.name();
                let (ptr, _elem_ty, _info) = self
                    .scope
                    .get(name)
                    .ok_or_else(|| CodeGenError::Llvm(format!("unknown identifier '{name}'")))?;
                Ok(self.builder.build_load(_elem_ty, ptr, name))
            }
            ExpressionNode::Binary(sp) => {
                let be = &sp.node;
                // Handle short-circuit logical operators specially
                if be.operator == BinaryOperator::And || be.operator == BinaryOperator::Or {
                    return self.codegen_short_circuit(be);
                }
                // Handle null-coalescing and elvis (truthy select between lhs and rhs)
                if be.operator == BinaryOperator::NullCoalesce
                    || be.operator == BinaryOperator::Elvis
                {
                    let lhs = self.codegen_expr(&be.left)?;
                    let rhs = self.codegen_expr(&be.right)?;
                    // Types must match for selection
                    let lhs_ty = Self::basic_type_of_value(&lhs);
                    let rhs_ty = Self::basic_type_of_value(&rhs);
                    if lhs_ty != rhs_ty {
                        return Err(CodeGenError::Llvm(
                            "null-coalescing/elvis operands must have same type".into(),
                        ));
                    }
                    let cond = self.to_bool(lhs)?;
                    // Create blocks
                    let func = self
                        .current_fn
                        .ok_or_else(|| CodeGenError::Llvm("?: outside function".into()))?;
                    let then_bb = self.context.append_basic_block(func, "sel.then");
                    let else_bb = self.context.append_basic_block(func, "sel.else");
                    let cont_bb = self.context.append_basic_block(func, "sel.end");
                    self.builder
                        .build_conditional_branch(cond, then_bb, else_bb);
                    // then path: yield lhs
                    self.builder.position_at_end(then_bb);
                    let then_val = lhs;
                    let then_end = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(cont_bb);
                    // else path: yield rhs
                    self.builder.position_at_end(else_bb);
                    let else_val = rhs;
                    let else_end = self.builder.get_insert_block().unwrap();
                    self.builder.build_unconditional_branch(cont_bb);
                    // cont with phi
                    self.builder.position_at_end(cont_bb);
                    let phi = self.builder.build_phi(lhs_ty, "sel.tmp");
                    phi.add_incoming(&[(&then_val, then_end), (&else_val, else_end)]);
                    return Ok(phi.as_basic_value());
                }
                // Handle placeholder UnitConversion: value → factor (numeric RHS only)
                if be.operator == BinaryOperator::UnitConversion {
                    let lhs = self.codegen_expr(&be.left)?;
                    // Only support numeric literal RHS for placeholder
                    let factor_val = match &be.right {
                        ExpressionNode::Literal(sp) => match &sp.node {
                            LiteralNode::Int(i) => Some((true, *i as f64)),
                            LiteralNode::Float(f) => Some((false, *f)),
                            _ => None,
                        },
                        _ => None,
                    }
                    .ok_or_else(|| {
                        CodeGenError::Llvm(
                            "unit conversion placeholder requires numeric RHS".into(),
                        )
                    })?;
                    match (lhs, factor_val) {
                        (BasicValueEnum::IntValue(li), (true, fi)) => {
                            let fac = self.i64.const_int(fi as u64, true);
                            return Ok(self.builder.build_int_mul(li, fac, "uconv.i").into());
                        }
                        (BasicValueEnum::FloatValue(lf), (_, ff)) => {
                            let fac = self.f64.const_float(ff);
                            return Ok(self.builder.build_float_mul(lf, fac, "uconv.f").into());
                        }
                        _ => {
                            return Err(CodeGenError::Llvm(
                                "unit conversion placeholder supports int/float LHS".into(),
                            ))
                        }
                    }
                }
                let lhs = self.codegen_expr(&be.left)?;
                let rhs = self.codegen_expr(&be.right)?;
                self.codegen_binop(be.operator, lhs, rhs)
            }
            ExpressionNode::HealthcareQuery(sp) => {
                let q = &sp.node;
                let callee_name = q.query_type.clone();
                let func = self.module.get_function(&callee_name).ok_or_else(|| {
                    CodeGenError::Llvm(format!("unknown healthcare query function '{callee_name}'"))
                })?;
                let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(q.arguments.len());
                for a in &q.arguments {
                    args.push(self.codegen_expr(a)?.into());
                }
                let call_site = self.builder.build_call(func, &args, "hcq");
                if func.get_type().get_return_type().is_none() {
                    Ok(self.i64.const_zero().into())
                } else {
                    Ok(call_site.try_as_basic_value().left().ok_or_else(|| {
                        CodeGenError::Llvm("expected value from healthcare query".into())
                    })?)
                }
            }
            ExpressionNode::Struct(sp) => {
                // Build a temporary struct aggregate and return it as a value
                let fields = &sp.node.fields;
                let mut vals: Vec<BasicValueEnum> = Vec::with_capacity(fields.len());
                let mut ftypes: Vec<BasicTypeEnum> = Vec::with_capacity(fields.len());
                let mut fnames: Vec<String> = Vec::with_capacity(fields.len());
                for f in fields {
                    let v = self.codegen_expr(&f.value)?;
                    ftypes.push(Self::basic_type_of_value(&v));
                    vals.push(v);
                    fnames.push(f.name.to_string());
                }
                let st =
                    self.get_or_define_struct(&sp.node.type_name.to_string(), &ftypes, &fnames)?;
                let tmp = self.create_entry_alloca("tmp.struct", st.into());
                self.store_struct_fields(st, tmp, &vals)?;
                Ok(self.builder.build_load(st, tmp, "tmp.struct.load"))
            }
            ExpressionNode::Member(sp) => {
                // Support `ident.member` and `(StructLiteral).member`
                let obj = &sp.node.object;
                let prop = &sp.node.property.name();
                match obj {
                    ExpressionNode::Identifier(id) => {
                        let var_name = id.node.name();
                        let (ptr, _elem_ty, info) = self.scope.get(var_name).ok_or_else(|| {
                            CodeGenError::Llvm(format!("unknown identifier '{var_name}'"))
                        })?;
                        let info = info.ok_or_else(|| {
                            CodeGenError::Llvm(format!("identifier '{var_name}' is not known to be a struct (no struct metadata)"))
                        })?;
                        let (st, field_names) = self
                            .struct_types
                            .get(&info.type_name)
                            .cloned()
                            .ok_or_else(|| {
                                CodeGenError::Llvm("unknown struct type in registry".into())
                            })?;
                        let Some(idx) = field_names.iter().position(|n| n == prop) else {
                            return Err(CodeGenError::Llvm(format!(
                                "struct '{}' has no field '{prop}'",
                                info.type_name
                            )));
                        };
                        let fld_ptr = self
                            .builder
                            .build_struct_gep(st, ptr, idx as u32, &format!("{var_name}.{prop}"))
                            .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                        Ok(self.builder.build_load(
                            st.get_field_types()[idx],
                            fld_ptr,
                            &format!("load.{var_name}.{prop}"),
                        ))
                    }
                    ExpressionNode::Struct(slit) => {
                        // Build temp struct and load field by name from the literal's declared order
                        let fields = &slit.node.fields;
                        let mut vals: Vec<BasicValueEnum> = Vec::with_capacity(fields.len());
                        let mut ftypes: Vec<BasicTypeEnum> = Vec::with_capacity(fields.len());
                        let mut fnames: Vec<String> = Vec::with_capacity(fields.len());
                        for f in fields {
                            let v = self.codegen_expr(&f.value)?;
                            ftypes.push(Self::basic_type_of_value(&v));
                            vals.push(v);
                            fnames.push(f.name.to_string());
                        }
                        let st = self.get_or_define_struct(
                            &slit.node.type_name.to_string(),
                            &ftypes,
                            &fnames,
                        )?;
                        let tmp = self.create_entry_alloca("tmp.struct.obj", st.into());
                        self.store_struct_fields(st, tmp, &vals)?;
                        let Some(idx) = fnames.iter().position(|n| n == prop) else {
                            return Err(CodeGenError::Llvm(format!(
                                "struct literal '{}' has no field '{prop}'",
                                slit.node.type_name
                            )));
                        };
                        let fld_ptr = self
                            .builder
                            .build_struct_gep(st, tmp, idx as u32, &format!("lit.{prop}"))
                            .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                        Ok(self.builder.build_load(
                            st.get_field_types()[idx],
                            fld_ptr,
                            &format!("load.lit.{prop}"),
                        ))
                    }
                    _ => Err(CodeGenError::Llvm(
                        "member access supported only on identifiers or struct literals currently"
                            .into(),
                    )),
                }
            }
            ExpressionNode::Array(ap) => {
                // Build a temporary fixed-size array and return it as a value
                let mut vals: Vec<BasicValueEnum> = Vec::with_capacity(ap.node.elements.len());
                for el in &ap.node.elements {
                    vals.push(self.codegen_expr(el)?);
                }
                let arr_ty = self.infer_array_type(&vals)?;
                let tmp = self.create_entry_alloca("tmp.array", arr_ty.into());
                self.store_array_elements(arr_ty, tmp, &vals)?;
                Ok(self.builder.build_load(arr_ty, tmp, "tmp.array.load"))
            }
            ExpressionNode::Call(sp) => {
                let call = &sp.node;
                // Only support direct identifier calls for now
                let callee_name = match &call.callee {
                    ExpressionNode::Identifier(id) => id.node.name().to_string(),
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "only simple function calls supported".into(),
                        ))
                    }
                };
                let func = self.module.get_function(&callee_name).ok_or_else(|| {
                    CodeGenError::Llvm(format!("unknown function '{callee_name}'"))
                })?;
                let mut args: Vec<BasicMetadataValueEnum> =
                    Vec::with_capacity(call.arguments.len());
                for a in &call.arguments {
                    let v = self.codegen_expr(a)?;
                    args.push(v.into());
                }
                let call_site = self.builder.build_call(func, &args, "calltmp");
                // If the function returns void, synthesize a 0 i64 value to keep expression type expectations simple
                if func.get_type().get_return_type().is_none() {
                    Ok(self.i64.const_zero().into())
                } else {
                    Ok(call_site
                        .try_as_basic_value()
                        .left()
                        .ok_or_else(|| CodeGenError::Llvm("expected value from call".into()))?)
                }
            }
            // Unsupported expressions (others above are already handled)
            ExpressionNode::Statement(_)
            | ExpressionNode::IcdCode(_)
            | ExpressionNode::CptCode(_)
            | ExpressionNode::SnomedCode(_) => Err(CodeGenError::Llvm(
                "unsupported expression in codegen".into(),
            )),
        }
    }

    fn codegen_binop(
        &mut self,
        op: BinaryOperator,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        use BinaryOperator as Op;
        let err = || CodeGenError::Llvm("type mismatch in binary op".into());
        match (lhs, rhs) {
            (BasicValueEnum::IntValue(li), BasicValueEnum::IntValue(ri)) => {
                let v: BasicValueEnum = match op {
                    Op::Add => self.builder.build_int_add(li, ri, "iadd").into(),
                    Op::Sub => self.builder.build_int_sub(li, ri, "isub").into(),
                    Op::Mul => self.builder.build_int_mul(li, ri, "imul").into(),
                    Op::Div => self.builder.build_int_signed_div(li, ri, "idiv").into(),
                    Op::Mod => self.builder.build_int_signed_rem(li, ri, "irem").into(),
                    // Medical/domain operators mapping
                    Op::Of => self.builder.build_int_mul(li, ri, "iof").into(),
                    Op::Per => self.builder.build_int_signed_div(li, ri, "iper").into(),
                    // Exponentiation for integers via repeated multiplication (simple, no negatives)
                    Op::Pow => {
                        // rhs is exponent; implement loop: res=1; for i in 0..exp { res*=base }
                        let func = self
                            .current_fn
                            .ok_or_else(|| CodeGenError::Llvm("pow outside function".into()))?;
                        let pre_bb = self.context.append_basic_block(func, "ipow.pre");
                        let loop_bb = self.context.append_basic_block(func, "ipow.loop");
                        let cont_bb = self.context.append_basic_block(func, "ipow.end");
                        self.builder.build_unconditional_branch(pre_bb);
                        self.builder.position_at_end(pre_bb);
                        let res_alloca = self.create_entry_alloca("ipow.res", li.get_type().into());
                        let i_alloca = self.create_entry_alloca("ipow.i", ri.get_type().into());
                        self.builder
                            .build_store(res_alloca, li.get_type().const_int(1, false));
                        self.builder
                            .build_store(i_alloca, ri.get_type().const_zero());
                        self.builder.build_unconditional_branch(loop_bb);
                        // loop: if i < exp then res*=base; i++
                        self.builder.position_at_end(loop_bb);
                        let i_cur = self
                            .builder
                            .build_load(ri.get_type(), i_alloca, "i")
                            .into_int_value();
                        let cmp = self.builder.build_int_compare(
                            inkwell::IntPredicate::ULT,
                            i_cur,
                            ri,
                            "icmp",
                        );
                        let body_bb = self.context.append_basic_block(func, "ipow.body");
                        self.builder.build_conditional_branch(cmp, body_bb, cont_bb);
                        self.builder.position_at_end(body_bb);
                        let res_cur = self
                            .builder
                            .build_load(li.get_type(), res_alloca, "res")
                            .into_int_value();
                        let res_next = self.builder.build_int_mul(res_cur, li, "res.next");
                        self.builder.build_store(res_alloca, res_next);
                        let i_next = self.builder.build_int_add(
                            i_cur,
                            ri.get_type().const_int(1, false),
                            "i.next",
                        );
                        self.builder.build_store(i_alloca, i_next);
                        self.builder.build_unconditional_branch(loop_bb);
                        self.builder.position_at_end(cont_bb);
                        let res_final =
                            self.builder
                                .build_load(li.get_type(), res_alloca, "res.final");
                        res_final
                    }
                    Op::BitAnd => self.builder.build_and(li, ri, "iand").into(),
                    Op::BitOr => self.builder.build_or(li, ri, "ior").into(),
                    Op::BitXor => self.builder.build_xor(li, ri, "ixor").into(),
                    Op::Shl => self.builder.build_left_shift(li, ri, "ishl").into(),
                    Op::Shr => self.builder.build_right_shift(li, ri, true, "ishr").into(),
                    Op::Eq => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, li, ri, "icmp_eq")
                        .into(),
                    Op::Ne => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::NE, li, ri, "icmp_ne")
                        .into(),
                    Op::Lt => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLT, li, ri, "icmp_lt")
                        .into(),
                    Op::Le => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SLE, li, ri, "icmp_le")
                        .into(),
                    Op::Gt => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGT, li, ri, "icmp_gt")
                        .into(),
                    Op::Ge => self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::SGE, li, ri, "icmp_ge")
                        .into(),
                    _ => return Err(CodeGenError::Llvm("unsupported int operator".into())),
                };
                Ok(v)
            }
            (BasicValueEnum::FloatValue(lf), BasicValueEnum::FloatValue(rf)) => {
                let v: BasicValueEnum = match op {
                    Op::Add => self.builder.build_float_add(lf, rf, "fadd").into(),
                    Op::Sub => self.builder.build_float_sub(lf, rf, "fsub").into(),
                    Op::Mul => self.builder.build_float_mul(lf, rf, "fmul").into(),
                    Op::Div => self.builder.build_float_div(lf, rf, "fdiv").into(),
                    Op::Of => self.builder.build_float_mul(lf, rf, "fof").into(),
                    Op::Per => self.builder.build_float_div(lf, rf, "fper").into(),
                    Op::Pow => {
                        // Use llvm.pow.f64 intrinsic: declare if missing
                        let pow_name = "llvm.pow.f64";
                        let fn_ty = self.f64.fn_type(&[self.f64.into(), self.f64.into()], false);
                        let pow_fn = match self.module.get_function(pow_name) {
                            Some(f) => f,
                            None => self.module.add_function(pow_name, fn_ty, None),
                        };
                        let cs = self
                            .builder
                            .build_call(pow_fn, &[lf.into(), rf.into()], "fpow");
                        cs.try_as_basic_value()
                            .left()
                            .ok_or_else(|| CodeGenError::Llvm("expected value from pow".into()))?
                    }
                    Op::Eq => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OEQ, lf, rf, "fcmp_eq")
                        .as_basic_value_enum(),
                    Op::Ne => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::ONE, lf, rf, "fcmp_ne")
                        .as_basic_value_enum(),
                    Op::Lt => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OLT, lf, rf, "fcmp_lt")
                        .as_basic_value_enum(),
                    Op::Le => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OLE, lf, rf, "fcmp_le")
                        .as_basic_value_enum(),
                    Op::Gt => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OGT, lf, rf, "fcmp_gt")
                        .as_basic_value_enum(),
                    Op::Ge => self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OGE, lf, rf, "fcmp_ge")
                        .as_basic_value_enum(),
                    _ => return Err(CodeGenError::Llvm("unsupported float operator".into())),
                };
                Ok(v)
            }
            _ => Err(err()),
        }
    }

    fn codegen_stmt(&mut self, stmt: &StatementNode) -> Result<(), CodeGenError> {
        match stmt {
            StatementNode::Let(letn) => {
                let ty = if let Some(expr) = &letn.type_annotation {
                    // Prefer named type annotations like int/float/bool/string; else fall back to literal inference
                    if let Some(t) = self.type_from_annotation(expr) {
                        t
                    } else if let ExpressionNode::Literal(sp) = expr {
                        self.basic_type_for_literal(&sp.node)
                    } else {
                        self.i64.into()
                    }
                } else {
                    self.i64.into()
                };
                // Special-case struct literal initializer: allocate as struct and store fields
                if let Some(ExpressionNode::Struct(sp)) = &letn.value {
                    let fields = &sp.node.fields;
                    // Evaluate field values in declared order
                    let mut vals: Vec<BasicValueEnum> = Vec::with_capacity(fields.len());
                    let mut ftypes: Vec<BasicTypeEnum> = Vec::with_capacity(fields.len());
                    let mut fnames: Vec<String> = Vec::with_capacity(fields.len());
                    for f in fields {
                        let v = self.codegen_expr(&f.value)?;
                        ftypes.push(Self::basic_type_of_value(&v));
                        vals.push(v);
                        fnames.push(f.name.to_string());
                    }
                    let st = self.get_or_define_struct(
                        &sp.node.type_name.to_string(),
                        &ftypes,
                        &fnames,
                    )?;
                    let var_ptr = self.create_entry_alloca(letn.name.name(), st.into());
                    self.store_struct_fields(st, var_ptr, &vals)?;
                    self.scope.insert_with_struct(
                        letn.name.name(),
                        var_ptr,
                        st.into(),
                        StructInfo {
                            type_name: sp.node.type_name.to_string(),
                            field_names: fnames,
                        },
                    );
                } else if let Some(ExpressionNode::Array(ap)) = &letn.value {
                    // Array literal initializer: infer element type, allocate array, store elements
                    let mut vals: Vec<BasicValueEnum> = Vec::with_capacity(ap.node.elements.len());
                    for el in &ap.node.elements {
                        vals.push(self.codegen_expr(el)?);
                    }
                    let arr_ty = self.infer_array_type(&vals)?;
                    let var_ptr = self.create_entry_alloca(letn.name.name(), arr_ty.into());
                    self.store_array_elements(arr_ty, var_ptr, &vals)?;
                    self.scope.insert(letn.name.name(), var_ptr, arr_ty.into());
                } else {
                    let ptr = self.create_entry_alloca(letn.name.name(), ty);
                    self.scope.insert(letn.name.name(), ptr, ty);
                    if let Some(init) = &letn.value {
                        let v = self.codegen_expr(init)?;
                        self.builder.build_store(ptr, v);
                    }
                }
                Ok(())
            }
            StatementNode::Assignment(assign) => {
                match &assign.target {
                    ExpressionNode::Identifier(id) => {
                        let name = id.node.name();
                        let (ptr, elem_ty, info) = self.scope.get(name).ok_or_else(|| {
                            CodeGenError::Llvm(format!("unknown identifier '{name}'"))
                        })?;
                        if let ExpressionNode::Struct(sp) = &assign.value {
                            // Assign struct literal into existing struct variable
                            let (st, field_names) = if let Some(si) = info {
                                self.struct_types
                                    .get(&si.type_name)
                                    .cloned()
                                    .ok_or_else(|| {
                                        CodeGenError::Llvm("unknown struct type in registry".into())
                                    })?
                            } else {
                                return Err(CodeGenError::Llvm(
                                    "cannot assign struct literal to non-struct variable".into(),
                                ));
                            };
                            let mut vals: Vec<BasicValueEnum> =
                                Vec::with_capacity(field_names.len());
                            let mut src_map = std::collections::HashMap::new();
                            for f in &sp.node.fields {
                                src_map.insert(f.name.to_string(), &f.value);
                            }
                            for fname in &field_names {
                                let expr = src_map.get(fname).ok_or_else(|| {
                                    CodeGenError::Llvm(format!(
                                        "missing field '{fname}' in struct assignment"
                                    ))
                                })?;
                                vals.push(self.codegen_expr(expr)?);
                            }
                            self.store_struct_fields(st, ptr, &vals)?;
                        } else if let ExpressionNode::Array(ap) = &assign.value {
                            // Assign array literal into existing array variable
                            let BasicTypeEnum::ArrayType(arr_ty) = elem_ty else {
                                return Err(CodeGenError::Llvm(
                                    "cannot assign array literal to non-array variable".into(),
                                ));
                            };
                            let mut vals: Vec<BasicValueEnum> =
                                Vec::with_capacity(ap.node.elements.len());
                            for el in &ap.node.elements {
                                vals.push(self.codegen_expr(el)?);
                            }
                            if vals.len() as u32 != arr_ty.len() {
                                return Err(CodeGenError::Llvm(
                                    "array length mismatch in assignment".into(),
                                ));
                            }
                            let expected_elem_ty = arr_ty.get_element_type();
                            for v in &vals {
                                if Self::basic_type_of_value(v) != expected_elem_ty {
                                    return Err(CodeGenError::Llvm(
                                        "array element type mismatch in assignment".into(),
                                    ));
                                }
                            }
                            self.store_array_elements(arr_ty, ptr, &vals)?;
                        } else {
                            let v = self.codegen_expr(&assign.value)?;
                            self.builder.build_store(ptr, v);
                        }
                        Ok(())
                    }
                    ExpressionNode::Member(m) => {
                        // Assignment to a struct field: obj.field = value
                        let obj = &m.node.object;
                        let prop = m.node.property.name();
                        let (st, field_names, base_ptr) = match obj {
                            ExpressionNode::Identifier(id) => {
                                let var_name = id.node.name();
                                let (ptr, _ty, info) =
                                    self.scope.get(var_name).ok_or_else(|| {
                                        CodeGenError::Llvm(format!(
                                            "unknown identifier '{var_name}'"
                                        ))
                                    })?;
                                let info = info.ok_or_else(|| {
                                    CodeGenError::Llvm("non-struct member assignment".into())
                                })?;
                                let (st, names) =
                                    self.struct_types.get(&info.type_name).cloned().ok_or_else(
                                        || CodeGenError::Llvm("unknown struct type".into()),
                                    )?;
                                (st, names, ptr)
                            }
                            _ => {
                                return Err(CodeGenError::Llvm(
                                    "unsupported member assignment target".into(),
                                ))
                            }
                        };
                        let Some(idx) = field_names.iter().position(|n| n == prop) else {
                            return Err(CodeGenError::Llvm(format!(
                                "struct has no field '{prop}'"
                            )));
                        };
                        let fld_ptr = self
                            .builder
                            .build_struct_gep(
                                st,
                                base_ptr,
                                idx as u32,
                                &format!("store.{prop}.{idx}"),
                            )
                            .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                        let val = self.codegen_expr(&assign.value)?;
                        self.builder.build_store(fld_ptr, val);
                        Ok(())
                    }
                    _ => Err(CodeGenError::Llvm(
                        "assignment target must be identifier or member".into(),
                    )),
                }
            }
            StatementNode::Expr(expr) => {
                let _ = self.codegen_expr(expr)?;
                Ok(())
            }
            StatementNode::Block(block) => {
                self.scope.push();
                for s in &block.statements {
                    self.codegen_stmt(s)?;
                }
                self.scope.pop();
                Ok(())
            }
            StatementNode::If(ifn) => {
                let parent = self
                    .current_fn
                    .ok_or_else(|| CodeGenError::Llvm("if outside function".into()))?;
                let then_bb = self.context.append_basic_block(parent, "then");
                let cont_bb = self.context.append_basic_block(parent, "endif");
                let else_bb = if ifn.else_branch.is_some() {
                    Some(self.context.append_basic_block(parent, "else"))
                } else {
                    None
                };

                // condition: expect i1
                let cond_val = self.codegen_expr(&ifn.condition)?;
                let cond_i1: IntValue = self.to_bool(cond_val)?;
                self.builder
                    .build_conditional_branch(cond_i1, then_bb, else_bb.unwrap_or(cont_bb));

                // then
                self.builder.position_at_end(then_bb);
                self.codegen_stmt(&StatementNode::Block(Box::new(ifn.then_branch.clone())))?;
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder.build_unconditional_branch(cont_bb);
                }

                // else
                if let Some(else_stmt) = &ifn.else_branch {
                    let else_bb = else_bb.expect("else bb");
                    self.builder.position_at_end(else_bb);
                    self.codegen_stmt(else_stmt)?;
                    if self
                        .builder
                        .get_insert_block()
                        .unwrap()
                        .get_terminator()
                        .is_none()
                    {
                        self.builder.build_unconditional_branch(cont_bb);
                    }
                }

                // continue
                self.builder.position_at_end(cont_bb);
                Ok(())
            }
            StatementNode::While(wh) => {
                let parent = self
                    .current_fn
                    .ok_or_else(|| CodeGenError::Llvm("while outside function".into()))?;
                let cond_bb = self.context.append_basic_block(parent, "loop.cond");
                let body_bb = self.context.append_basic_block(parent, "loop.body");
                let cont_bb = self.context.append_basic_block(parent, "loop.end");
                self.builder.build_unconditional_branch(cond_bb);
                // cond
                self.builder.position_at_end(cond_bb);
                let cond_val = self.codegen_expr(&wh.condition)?;
                let cond_i1: IntValue = self.to_bool(cond_val)?;
                self.builder
                    .build_conditional_branch(cond_i1, body_bb, cont_bb);
                // body
                self.builder.position_at_end(body_bb);
                self.codegen_stmt(&StatementNode::Block(Box::new(wh.body.clone())))?;
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder.build_unconditional_branch(cond_bb);
                }
                // cont
                self.builder.position_at_end(cont_bb);
                Ok(())
            }
            StatementNode::For(fr) => {
                // Basic lowering for integer range: for x in a .. b { body }
                // Only supports `a .. b` with integer operands.
                let parent = self
                    .current_fn
                    .ok_or_else(|| CodeGenError::Llvm("for outside function".into()))?;
                // Parse iterable as a range
                let (start_v, end_v) = match &fr.iterable {
                    ExpressionNode::Binary(sp) if sp.node.operator == BinaryOperator::Range => (
                        self.codegen_expr(&sp.node.left)?,
                        self.codegen_expr(&sp.node.right)?,
                    ),
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "for-loop currently supports only integer ranges: start .. end".into(),
                        ))
                    }
                };
                let (start_i, end_i) = match (start_v, end_v) {
                    (BasicValueEnum::IntValue(si), BasicValueEnum::IntValue(ei)) => (si, ei),
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "for-loop range bounds must be integers".into(),
                        ))
                    }
                };

                // Allocate loop variable in current scope if not present; shadow if exists
                let var_name = fr.variable.name();
                let var_ptr = self.create_entry_alloca(var_name, self.i64.into());
                self.scope.insert(var_name, var_ptr, self.i64.into());
                self.builder.build_store(var_ptr, start_i);

                // Create blocks
                let cond_bb = self.context.append_basic_block(parent, "for.cond");
                let body_bb = self.context.append_basic_block(parent, "for.body");
                let step_bb = self.context.append_basic_block(parent, "for.step");
                let cont_bb = self.context.append_basic_block(parent, "for.end");
                self.builder.build_unconditional_branch(cond_bb);

                // Condition: i < end
                self.builder.position_at_end(cond_bb);
                let cur_i = self
                    .builder
                    .build_load(self.i64, var_ptr, var_name)
                    .into_int_value();
                let cmp = self.builder.build_int_compare(
                    inkwell::IntPredicate::SLT,
                    cur_i,
                    end_i,
                    "for.cmp",
                );
                self.builder.build_conditional_branch(cmp, body_bb, cont_bb);

                // Body
                self.builder.position_at_end(body_bb);
                self.scope.push();
                self.codegen_stmt(&StatementNode::Block(Box::new(fr.body.clone())))?;
                self.scope.pop();
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    self.builder.build_unconditional_branch(step_bb);
                }

                // Step: i = i + 1
                self.builder.position_at_end(step_bb);
                let cur_i2 = self
                    .builder
                    .build_load(self.i64, var_ptr, var_name)
                    .into_int_value();
                let next =
                    self.builder
                        .build_int_add(cur_i2, self.i64.const_int(1, false), "for.next");
                self.builder.build_store(var_ptr, next);
                self.builder.build_unconditional_branch(cond_bb);

                // Continue
                self.builder.position_at_end(cont_bb);
                Ok(())
            }
            StatementNode::Return(ret) => {
                if let Some(v) = &ret.value {
                    let rv = self.codegen_expr(v)?;
                    self.builder.build_return(Some(&rv));
                } else {
                    self.builder.build_return(None);
                }
                Ok(())
            }
            StatementNode::Function(func) => {
                // Very simple convention: if return_type is Some => i64, else void
                // Determine parameter and return types from annotations when available
                let mut param_tys: Vec<BasicMetadataTypeEnum> =
                    Vec::with_capacity(func.params.len());
                for p in &func.params {
                    let ty = if let Some(ann) = &p.type_annotation {
                        self.type_from_annotation(ann).unwrap_or(self.i64.into())
                    } else {
                        self.i64.into()
                    };
                    param_tys.push(BasicMetadataTypeEnum::from(ty));
                }
                let fn_type = if let Some(ret_ann) = &func.return_type {
                    let rty = self
                        .type_from_annotation(ret_ann)
                        .unwrap_or(self.i64.into());
                    match rty {
                        BasicTypeEnum::IntType(t) => t.fn_type(&param_tys, false),
                        BasicTypeEnum::FloatType(t) => t.fn_type(&param_tys, false),
                        BasicTypeEnum::PointerType(t) => t.fn_type(&param_tys, false),
                        BasicTypeEnum::ArrayType(t) => t.fn_type(&param_tys, false),
                        BasicTypeEnum::StructType(t) => t.fn_type(&param_tys, false),
                        BasicTypeEnum::VectorType(t) => t.fn_type(&param_tys, false),
                    }
                } else {
                    self.void.fn_type(&param_tys, false)
                };
                let name = func.name.name();
                let function = self.module.add_function(name, fn_type, None);

                // Entry block and parameter stores
                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);
                let prev_fn = self.current_fn.replace(function);
                self.scope.push();
                // Allocate params with their actual LLVM parameter types
                let fn_param_tys = function.get_type().get_param_types();
                for (idx, p) in func.params.iter().enumerate() {
                    let llvm_param = function.get_nth_param(idx as u32).expect("param");
                    let param_ty = fn_param_tys.get(idx).copied().unwrap_or(self.i64.into());
                    let alloca = self.create_entry_alloca(p.name.name(), param_ty);
                    self.builder.build_store(alloca, llvm_param);
                    self.scope.insert(p.name.name(), alloca, param_ty);
                }

                // Body
                self.codegen_stmt(&StatementNode::Block(Box::new(func.body.clone())))?;

                // If no terminator, add default return
                if self
                    .builder
                    .get_insert_block()
                    .unwrap()
                    .get_terminator()
                    .is_none()
                {
                    // Default return if missing: zero of return type or void
                    match function.get_type().get_return_type() {
                        Some(ret) => match ret {
                            BasicTypeEnum::IntType(t) => {
                                let z = t.const_zero();
                                self.builder.build_return(Some(&z));
                            }
                            BasicTypeEnum::FloatType(t) => {
                                let z = t.const_zero();
                                self.builder.build_return(Some(&z));
                            }
                            _ => {
                                // Unknown/aggregate return: no sensible zero here; emit void return
                                self.builder.build_return(None);
                            }
                        },
                        None => {
                            self.builder.build_return(None);
                        }
                    }
                }
                self.scope.pop();
                self.current_fn = prev_fn;
                Ok(())
            }
            StatementNode::TypeDecl(td) => {
                // Register or validate a named struct layout by field names.
                let type_name = td.name.name().to_string();
                let field_names: Vec<String> =
                    td.fields.iter().map(|f| f.name.to_string()).collect();
                if let Some((existing_st, existing_names)) = self.struct_types.get(&type_name) {
                    if &field_names != existing_names {
                        return Err(CodeGenError::Llvm(format!(
                            "type '{type_name}' re-declared with different field names"
                        )));
                    }
                    // Already registered; nothing more to do (types set on first concrete use)
                    let _ = existing_st;
                } else {
                    // Create opaque struct now; we'll set body on first concrete use (e.g., struct literal)
                    let st = self.context.opaque_struct_type(&type_name);
                    self.struct_types.insert(type_name, (st, field_names));
                }
                Ok(())
            }
            StatementNode::Match(m) => self.codegen_match(m),
        }
    }

    fn codegen_match(&mut self, m: &MatchNode) -> Result<(), CodeGenError> {
        let func = self
            .current_fn
            .ok_or_else(|| CodeGenError::Llvm("match outside function".into()))?;
        let scrut = self.codegen_expr(&m.expr)?;
        let cont_bb = self.context.append_basic_block(func, "match.end");

        // Create a chain of test blocks
        let mut next_test_bb: Option<inkwell::basic_block::BasicBlock> = None;
        for (i, arm) in m.arms.iter().enumerate() {
            let test_bb = self
                .context
                .append_basic_block(func, &format!("match.test.{i}"));
            let body_bb = self
                .context
                .append_basic_block(func, &format!("match.body.{i}"));
            let after_test_bb = self
                .context
                .append_basic_block(func, &format!("match.next.{i}"));

            // Link previous to this test
            match next_test_bb.take() {
                Some(prev) => {
                    self.builder.position_at_end(prev);
                    self.builder.build_unconditional_branch(test_bb);
                }
                None => {
                    // First test comes from current position
                    if self.builder.get_insert_block().is_none() {
                        // Should not happen; position to end of last BB
                        let entry = func
                            .get_first_basic_block()
                            .ok_or_else(|| CodeGenError::Llvm("function without entry".into()))?;
                        self.builder.position_at_end(entry);
                    }
                    self.builder.build_unconditional_branch(test_bb);
                }
            }

            // Build test
            self.builder.position_at_end(test_bb);
            let cond = self.pattern_matches(scrut, &arm.pattern)?;
            self.builder
                .build_conditional_branch(cond, body_bb, after_test_bb);

            // Body: perform bindings for identifiers in the pattern
            self.builder.position_at_end(body_bb);
            self.scope.push();
            self.bind_pattern(scrut, &arm.pattern)?;
            // Evaluate arm body expression; discard result
            let _ = self.codegen_expr(&arm.body)?;
            self.scope.pop();
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
            {
                self.builder.build_unconditional_branch(cont_bb);
            }

            // Prepare next test
            next_test_bb = Some(after_test_bb);
        }

        // If no arm matched, jump to cont
        if let Some(last_test) = next_test_bb {
            self.builder.position_at_end(last_test);
            self.builder.build_unconditional_branch(cont_bb);
        }

        // Continue after match
        self.builder.position_at_end(cont_bb);
        Ok(())
    }

    fn pattern_matches(
        &mut self,
        scrut: BasicValueEnum<'ctx>,
        pat: &PatternNode,
    ) -> Result<IntValue<'ctx>, CodeGenError> {
        match pat {
            PatternNode::Wildcard => Ok(self.i1.const_int(1, false)),
            PatternNode::Identifier(_) => Ok(self.i1.const_int(1, false)),
            PatternNode::Literal(lit) => {
                let lit_val = self.const_for_literal(lit);
                match (scrut, lit_val) {
                    (BasicValueEnum::IntValue(si), BasicValueEnum::IntValue(li)) => Ok(self
                        .builder
                        .build_int_compare(inkwell::IntPredicate::EQ, si, li, "match.eq")),
                    (BasicValueEnum::FloatValue(sf), BasicValueEnum::FloatValue(lf)) => Ok(self
                        .builder
                        .build_float_compare(inkwell::FloatPredicate::OEQ, sf, lf, "match.feq")),
                    _ => Err(CodeGenError::Llvm(
                        "match literal pattern only supports int/float/bool currently".into(),
                    )),
                }
            }
            PatternNode::Struct { type_name, fields } => {
                // Look up struct type by name
                let key = type_name.to_string();
                let (st, field_names) =
                    self.struct_types.get(&key).cloned().ok_or_else(|| {
                        CodeGenError::Llvm("unknown struct type in pattern".into())
                    })?;
                // For value-based matching, we need a pointer to the struct to access fields
                let tmp_ptr = self.create_entry_alloca("match.scrut.struct", st.into());
                self.builder.build_store(tmp_ptr, scrut);

                // Start with true, then AND each field comparison
                let mut acc = self.i1.const_int(1, false);
                for fpat in fields {
                    let fname = fpat.name.to_string();
                    let Some(idx) = field_names.iter().position(|n| n == &fname) else {
                        return Err(CodeGenError::Llvm(format!(
                            "struct pattern field '{fname}' not found in type '{key}'"
                        )));
                    };
                    let fld_ptr = self
                        .builder
                        .build_struct_gep(st, tmp_ptr, idx as u32, &format!("pat.{fname}"))
                        .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                    let fld_ty = st.get_field_types()[idx];
                    let fld_val =
                        self.builder
                            .build_load(fld_ty, fld_ptr, &format!("pat.load.{fname}"));
                    let cond = self.pattern_matches(fld_val, &fpat.pattern)?;
                    acc = self.builder.build_and(acc, cond, "match.struct.and");
                }
                Ok(acc)
            }
            PatternNode::Variant { name, inner } => {
                // Expect scrutinee as a struct with field0 = i32 tag, field1 = payload
                let tag_id = self.get_variant_tag(&name.to_string());
                // We need struct type from value. Require scrut as struct value
                let (st, tmp_ptr) = match scrut {
                    BasicValueEnum::StructValue(sv) => {
                        let st = sv.get_type();
                        let tmp = self.create_entry_alloca("v.scrut.struct", st.into());
                        self.builder.build_store(tmp, scrut);
                        (st, tmp)
                    }
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "variant pattern requires struct scrutinee (tagged union)".into(),
                        ))
                    }
                };
                // Load tag at index 0
                let tag_ptr = self
                    .builder
                    .build_struct_gep(st, tmp_ptr, 0, "v.tag")
                    .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                // Try as i32 first if struct field is int32, else int64
                let tag_field_ty = st.get_field_types()[0];
                let tag_val = match tag_field_ty {
                    BasicTypeEnum::IntType(it) if it.get_bit_width() == 32 => {
                        let ti = self.builder.build_load(it, tag_ptr, "tag").into_int_value();
                        // compare with i32 constant
                        let c = it.const_int(tag_id as u64, false);
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::EQ, ti, c, "v.tag.eq")
                    }
                    BasicTypeEnum::IntType(it) if it.get_bit_width() == 64 => {
                        let ti = self.builder.build_load(it, tag_ptr, "tag").into_int_value();
                        let c = it.const_int(tag_id as u64, false);
                        self.builder
                            .build_int_compare(inkwell::IntPredicate::EQ, ti, c, "v.tag.eq")
                    }
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "variant tag field must be int32 or int64".into(),
                        ))
                    }
                };
                // If tag matches, also match payload
                // Extract payload at index 1 and compare recursively
                let payload_ptr = self
                    .builder
                    .build_struct_gep(st, tmp_ptr, 1, "v.payload")
                    .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                let payload_ty = st.get_field_types()[1];
                let payload_val =
                    self.builder
                        .build_load(payload_ty, payload_ptr, "v.payload.load");
                let inner_ok = self.pattern_matches(payload_val, inner)?;
                Ok(self.builder.build_and(tag_val, inner_ok, "v.and"))
            }
        }
    }

    fn bind_pattern(
        &mut self,
        scrut: BasicValueEnum<'ctx>,
        pat: &PatternNode,
    ) -> Result<(), CodeGenError> {
        match pat {
            PatternNode::Wildcard | PatternNode::Literal(_) => Ok(()),
            PatternNode::Identifier(ident) => {
                let elem_ty = Self::basic_type_of_value(&scrut);
                let ptr = self.create_entry_alloca(ident.name(), elem_ty);
                self.builder.build_store(ptr, scrut);
                self.scope.insert(ident.name(), ptr, elem_ty);
                Ok(())
            }
            PatternNode::Variant { name: _, inner } => {
                // Assume struct { tag, payload } and bind only payload recursively
                let (st, tmp_ptr) = match scrut {
                    BasicValueEnum::StructValue(sv) => {
                        let st = sv.get_type();
                        let tmp = self.create_entry_alloca("v.bind.struct", st.into());
                        self.builder.build_store(tmp, scrut);
                        (st, tmp)
                    }
                    _ => {
                        return Err(CodeGenError::Llvm(
                            "variant bind requires struct scrutinee".into(),
                        ))
                    }
                };
                let payload_ptr = self
                    .builder
                    .build_struct_gep(st, tmp_ptr, 1, "v.bind.payload")
                    .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                let payload_ty = st.get_field_types()[1];
                let payload_val =
                    self.builder
                        .build_load(payload_ty, payload_ptr, "v.bind.payload.load");
                self.bind_pattern(payload_val, inner)
            }
            PatternNode::Struct { type_name, fields } => {
                let key = type_name.to_string();
                let (st, field_names) =
                    self.struct_types.get(&key).cloned().ok_or_else(|| {
                        CodeGenError::Llvm("unknown struct type in pattern".into())
                    })?;
                let tmp_ptr = self.create_entry_alloca("bind.scrut.struct", st.into());
                self.builder.build_store(tmp_ptr, scrut);
                for fpat in fields {
                    let fname = fpat.name.to_string();
                    let Some(idx) = field_names.iter().position(|n| n == &fname) else {
                        return Err(CodeGenError::Llvm(format!(
                            "struct pattern field '{fname}' not found in type '{key}'"
                        )));
                    };
                    let fld_ptr = self
                        .builder
                        .build_struct_gep(st, tmp_ptr, idx as u32, &format!("bind.{fname}"))
                        .map_err(|_| CodeGenError::Llvm("gep error".into()))?;
                    let fld_ty = st.get_field_types()[idx];
                    let fld_val =
                        self.builder
                            .build_load(fld_ty, fld_ptr, &format!("bind.load.{fname}"));
                    self.bind_pattern(fld_val, &fpat.pattern)?;
                }
                Ok(())
            }
        }
    }
}

/// Generate IR text for a parsed Medi program.
#[cfg(feature = "llvm")]
pub fn generate_ir_string(program: &ProgramNode) -> Result<String, CodeGenError> {
    initialize_targets()?;
    let context = Context::create();
    let mut cg = CodeGen::new(&context, "medi_module");

    // Top-level: allow function declarations and statements within an implicit main()
    // Create an implicit `main` if there are top-level non-function statements
    let has_top_stmts = program
        .statements
        .iter()
        .any(|s| !matches!(s, StatementNode::Function(_)));
    if has_top_stmts {
        // int main()
        let fn_type = cg.i64.fn_type(&[], false);
        let function = cg.module.add_function("main", fn_type, None);
        let entry = cg.context.append_basic_block(function, "entry");
        cg.builder.position_at_end(entry);
        cg.current_fn = Some(function);
        for s in &program.statements {
            match s {
                StatementNode::Function(_) => { /* handle below */ }
                other => cg.codegen_stmt(other)?,
            }
        }
        if cg
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            let zero_iv = cg.i64.const_int(0, false);
            let zero_bv: BasicValueEnum = zero_iv.into();
            cg.builder.build_return(Some(&zero_bv));
        }
        cg.current_fn = None;
        cg.scope = Scope::new();
    }

    // Emit declared functions
    for s in &program.statements {
        if let StatementNode::Function(_) = s {
            cg.codegen_stmt(s)?;
        }
    }

    Ok(cg.module.print_to_string().to_string())
}
