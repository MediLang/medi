//! LLVM backend integration for Medi compiler (feature-gated).
//!
//! This crate provides a thin wrapper around LLVM via Inkwell to:
//! - Host a global `LLVMContext` per codegen session
//! - Create and manage `Module` and `IRBuilder`
//! - Initialize targets (x86_64, WebAssembly, RISC-V)
//!
//! To enable, build with feature `llvm` and ensure LLVM 15.x is installed on your system.
//! Example:
//!   cargo build -p medic_codegen_llvm --features llvm

use std::marker::PhantomData;
use thiserror::Error;

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
    module: inkwell::module::Module<'ctx>,
    // Ensure 'ctx is used even when the feature is disabled
    _phantom: PhantomData<&'ctx ()>,
}

/// Instruction builder wrapper.
pub struct IRBuilder<'ctx> {
    #[cfg(feature = "llvm")]
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
        // Core initialization for target info/MC/Asm.
        Target::initialize_x86(&Default::default());
        Target::initialize_webassembly(&Default::default());
        Target::initialize_riscv(&Default::default());
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
/// The concrete AST types live in `medic_ast`; we accept JSON or an opaque handle later.
pub fn generate_llvm_ir(_ast_json: &str) -> Result<(), CodeGenError> {
    // For now, this is a skeleton that validates feature gating and target init.
    initialize_targets()?;
    let ctx = CodegenContext::new()?;
    let _module = create_module(&ctx, "medi_module")?;
    let _builder = create_builder(&ctx)?;
    Ok(())
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
