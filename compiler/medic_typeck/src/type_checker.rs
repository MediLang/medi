// Type checker for Medic language in Rust
// This is a basic structure; expand with more rules as needed

use medic_ast::ast::*;
use medic_ast::visit::Span;
use medic_env::env::TypeEnv;
use medic_type::traits::{ValidationCtx, ValidationError};
use medic_type::types::*;
use std::collections::HashMap;

pub struct TypeChecker<'a> {
    env: &'a mut TypeEnv,
    /// Side type table: maps (start,end) byte offsets of an expression span to its computed type
    type_table: HashMap<(usize, usize), MediType>,
    /// Optional validation context for UCUM/LOINC/SNOMED checks
    validation_ctx: Option<&'a ValidationCtx>,
    /// Collected validation/type errors encountered during expression checks
    errors: Vec<TypeError>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(env: &'a mut TypeEnv) -> Self {
        TypeChecker {
            env,
            type_table: HashMap::new(),
            validation_ctx: None,
            errors: Vec::new(),
        }
    }

    /// Provide a validation context for UCUM/LOINC/SNOMED-aware checks.
    pub fn with_validation_ctx(mut self, ctx: &'a ValidationCtx) -> Self {
        self.validation_ctx = Some(ctx);
        self
    }

    /// Borrow collected errors
    pub fn errors(&self) -> &Vec<TypeError> {
        &self.errors
    }

    /// Drain and return collected errors
    pub fn take_errors(&mut self) -> Vec<TypeError> {
        std::mem::take(&mut self.errors)
    }

    /// Returns an immutable view of the side type table.
    pub fn type_table(&self) -> &HashMap<(usize, usize), MediType> {
        &self.type_table
    }

    /// Looks up a computed type by a `Span` key.
    pub fn get_type_at_span(&self, span: &Span) -> Option<&MediType> {
        self.type_table.get(&(span.start, span.end))
    }

    /// Resolve known builtin type names (primitives and common healthcare types)
    /// to `MediType` even if they are not present in the `TypeEnv`.
    fn builtin_type_by_name(name: &str) -> Option<MediType> {
        match name {
            "Int" | "int" => Some(MediType::Int),
            "Float" | "float" | "Number" | "number" => Some(MediType::Float),
            "Bool" | "bool" => Some(MediType::Bool),
            "String" | "string" => Some(MediType::String),
            // Healthcare-specific
            "PatientId" | "patient_id" => Some(MediType::PatientId),
            "Vital" | "vital" => Some(MediType::Vital),
            "LabResult" | "lab_result" => Some(MediType::LabResult),
            "FHIRPatient" => Some(MediType::FHIRPatient),
            "Observation" => Some(MediType::Observation),
            "Diagnosis" | "diagnosis" => Some(MediType::Diagnosis),
            "Medication" | "medication" => Some(MediType::Medication),
            "MedicalRecord" | "medical_record" => Some(MediType::MedicalRecord),
            _ => None,
        }
    }

    /// Convert a type-annotation expression into a `MediType`.
    /// For now we accept identifiers that match builtin names or env-bound names.
    fn resolve_annotation_type(&mut self, ty_expr: &ExpressionNode) -> MediType {
        match ty_expr {
            ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                if let Some(t) = Self::builtin_type_by_name(ident.name()) {
                    t
                } else {
                    self.env
                        .get(ident.name())
                        .cloned()
                        .unwrap_or(MediType::Unknown)
                }
            }
            _ => MediType::Unknown,
        }
    }

    /// Infers and returns the type of a given expression node in the Medic language.
    ///
    /// This method analyzes the provided expression node and determines its type according to Medic's type system rules. It supports literals, identifiers, binary operations (including arithmetic, comparison, logical, medical, range, unit conversion, bitwise, and null-coalescing operators), function calls, member access on structs, and healthcare-specific query expressions. If the type cannot be determined or is unsupported, `MediType::Unknown` is returned.
    ///
    /// # Examples
    ///
    /// ```
    /// use medic_ast::ast::{ExpressionNode, IdentifierNode, Spanned};
    /// use medic_ast::visit::Span;
    /// use medic_env::env::TypeEnv;
    /// use medic_type::types::MediType;
    /// use medic_typeck::type_checker::TypeChecker;
    ///
    /// let mut env = TypeEnv::with_prelude();
    /// env.insert("x".to_string(), MediType::Int);
    /// let mut checker = TypeChecker::new(&mut env);
    /// let id = Spanned::new(
    ///     IdentifierNode { name: "x".to_string() },
    ///     Span { start: 0, end: 1, line: 1, column: 1 },
    /// );
    /// let expr = ExpressionNode::Identifier(id);
    /// assert_eq!(checker.check_expr(&expr), MediType::Int);
    /// ```
    pub fn check_expr(&mut self, expr: &ExpressionNode) -> MediType {
        let ty = match expr {
            ExpressionNode::IcdCode(Spanned { node: _, .. })
            | ExpressionNode::CptCode(Spanned { node: _, .. }) => MediType::String,
            ExpressionNode::SnomedCode(Spanned { node: code, .. }) => {
                if let Some(ctx) = self.validation_ctx {
                    if !ctx.is_valid_snomed(code) {
                        self.errors.push(TypeError::ValidationFailed(
                            ValidationError::UnknownCode {
                                system: "SNOMED",
                                code: code.clone(),
                            },
                        ));
                    }
                }
                MediType::String
            }
            ExpressionNode::Identifier(Spanned { node: name, .. }) => self
                .env
                .get(name.name())
                .cloned()
                .or_else(|| Self::builtin_type_by_name(name.name()))
                .unwrap_or(MediType::Unknown),
            ExpressionNode::Literal(Spanned { node: lit, .. }) => match lit {
                LiteralNode::Int(_) => MediType::Int,
                LiteralNode::Float(_) => MediType::Float,
                LiteralNode::Bool(_) => MediType::Bool,
                LiteralNode::String(_) => MediType::String,
            },
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                let left = self.check_expr(&bin.left);
                let right = self.check_expr(&bin.right);
                match bin.operator {
                    // Arithmetic operators
                    BinaryOperator::Add
                    | BinaryOperator::Sub
                    | BinaryOperator::Mul
                    | BinaryOperator::Div
                    | BinaryOperator::Mod
                    | BinaryOperator::Shl
                    | BinaryOperator::Shr => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else if left.is_numeric() && right.is_numeric() {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Power operator (always returns float)
                    BinaryOperator::Pow => {
                        if left.is_numeric() && right.is_numeric() {
                            MediType::Float
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Comparison operators
                    BinaryOperator::Eq
                    | BinaryOperator::Ne
                    | BinaryOperator::Lt
                    | BinaryOperator::Gt
                    | BinaryOperator::Le
                    | BinaryOperator::Ge => {
                        if left.is_comparable_with(&right) {
                            MediType::Bool
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Logical operators
                    BinaryOperator::And | BinaryOperator::Or => {
                        if left == MediType::Bool && right == MediType::Bool {
                            MediType::Bool
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Medical operators
                    BinaryOperator::Of | BinaryOperator::Per => {
                        // These operators are used for medical quantities
                        // For now, we'll assume they return the type of the left operand
                        // More specific type checking can be added later
                        left
                    }
                    // Range operator
                    BinaryOperator::Range => {
                        if left == right {
                            MediType::Range(Box::new(left))
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Unit conversion operator
                    BinaryOperator::UnitConversion => {
                        // Check if both sides are unit types that can be converted
                        // For now, just return the right-hand type
                        right
                    }
                    // Bitwise operators
                    BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => {
                        if left == MediType::Int && right == MediType::Int {
                            MediType::Int
                        } else {
                            MediType::Unknown
                        }
                    }
                    // Null-coalescing and Elvis operators
                    BinaryOperator::NullCoalesce | BinaryOperator::Elvis => {
                        // Return the type of the right operand
                        right
                    }
                    // Assignment operator
                    BinaryOperator::Assign => {
                        // The type of an assignment is the type of the right-hand side.
                        // Validate that the left side is an lvalue (Identifier or Member).
                        if let ExpressionNode::Binary(Spanned { node: bin_expr, .. }) = expr {
                            let lvalue_ok = matches!(
                                bin_expr.left,
                                ExpressionNode::Identifier(_) | ExpressionNode::Member(_)
                            );
                            if !lvalue_ok {
                                log::error!(
                                    "Left side of assignment must be an identifier or member expression"
                                );
                                MediType::Unknown
                            } else {
                                right
                            }
                        } else {
                            // This should never happen for a binary expression with assign operator
                            log::error!(
                                "Internal error: Expected binary expression for assignment"
                            );
                            MediType::Unknown
                        }
                    }
                }
            }
            ExpressionNode::Call(Spanned { node: call, .. }) => {
                let callee_type = self.check_expr(&call.callee);
                if let MediType::Function {
                    params,
                    return_type,
                } = callee_type
                {
                    if params.len() == call.arguments.len() {
                        // Check argument types
                        for (arg, param_type) in call.arguments.iter().zip(params.iter()) {
                            let arg_type = self.check_expr(arg);
                            if arg_type != *param_type {
                                return MediType::Unknown;
                            }
                        }
                        *return_type
                    } else {
                        MediType::Unknown
                    }
                } else {
                    MediType::Unknown
                }
            }
            ExpressionNode::Member(Spanned { node: mem, .. }) => {
                let object_type = self.check_expr(&mem.object);
                if let MediType::Struct(fields) = object_type {
                    fields
                        .get(mem.property.name())
                        .cloned()
                        .unwrap_or(MediType::Unknown)
                } else {
                    MediType::Unknown // Member access is only valid on structs
                }
            }
            ExpressionNode::HealthcareQuery(Spanned { node: query, .. }) => {
                match query.query_type.as_str() {
                    "PatientData" => MediType::Record(vec![
                        ("id".to_string(), MediType::Int),
                        ("name".to_string(), MediType::String),
                        ("age".to_string(), MediType::Int),
                        (
                            "conditions".to_string(),
                            MediType::List(Box::new(MediType::String)),
                        ),
                    ]),
                    "AppointmentData" => MediType::List(Box::new(MediType::Record(vec![
                        ("appointment_id".to_string(), MediType::Int),
                        ("date".to_string(), MediType::String),
                        ("doctor".to_string(), MediType::String),
                    ]))),
                    _ => MediType::Unknown, // Fallback for unsupported query types
                }
            }
            ExpressionNode::Array(Spanned { node: arr, .. }) => {
                if arr.elements.is_empty() {
                    MediType::List(Box::new(MediType::Unknown))
                } else {
                    // Infer a unified element type
                    let mut iter = arr.elements.iter().map(|e| self.check_expr(e));
                    let mut elem_ty = iter.next().unwrap_or(MediType::Unknown);
                    for t in iter {
                        if t == elem_ty {
                            continue;
                        }
                        // If numeric mix, promote to Float
                        if t.is_numeric() && elem_ty.is_numeric() {
                            elem_ty = MediType::Float;
                        } else {
                            // Heterogeneous types: fall back to Unknown
                            elem_ty = MediType::Unknown;
                            break;
                        }
                    }
                    MediType::List(Box::new(elem_ty))
                }
            }
            ExpressionNode::Statement(Spanned { node: stmt, .. }) => {
                // For statement expressions, check the inner statement
                match &**stmt {
                    StatementNode::Expr(expr) => self.check_expr(expr),
                    _ => MediType::Void, // Other statements don't produce values
                }
            }
            ExpressionNode::Struct(Spanned {
                node: struct_lit, ..
            }) => {
                // For struct literals, we return a struct type with field types
                // TODO: Look up the actual struct definition for more precise type checking
                let mut fields = std::collections::HashMap::new();
                for field in &struct_lit.fields {
                    let field_type = self.check_expr(&field.value);
                    // Convert IdentifierName to String for the Struct type map
                    fields.insert(field.name.to_string(), field_type);
                }
                // Heuristic: if fields contain (code: String, value: number, unit: String),
                // and a ValidationCtx is present, perform LOINC/UCUM/reference checks.
                if let Some(ctx) = self.validation_ctx {
                    use medic_ast::ast::LiteralNode;
                    let mut code_val: Option<String> = None;
                    let mut unit_val: Option<String> = None;
                    let mut value_num: Option<f64> = None;
                    for f in &struct_lit.fields {
                        match (f.name.as_str(), &f.value) {
                            (
                                "code",
                                ExpressionNode::Literal(Spanned {
                                    node: LiteralNode::String(s),
                                    ..
                                }),
                            ) => {
                                code_val = Some(s.clone());
                            }
                            (
                                "unit",
                                ExpressionNode::Literal(Spanned {
                                    node: LiteralNode::String(s),
                                    ..
                                }),
                            ) => {
                                unit_val = Some(s.clone());
                            }
                            (
                                "value",
                                ExpressionNode::Literal(Spanned {
                                    node: LiteralNode::Float(n),
                                    ..
                                }),
                            ) => {
                                value_num = Some(*n);
                            }
                            (
                                "value",
                                ExpressionNode::Literal(Spanned {
                                    node: LiteralNode::Int(n),
                                    ..
                                }),
                            ) => {
                                value_num = Some(*n as f64);
                            }
                            _ => {}
                        }
                    }
                    if let (Some(code), Some(unit), Some(val)) = (code_val, unit_val, value_num) {
                        if !ctx.is_valid_loinc(&code) {
                            self.errors.push(TypeError::ValidationFailed(
                                ValidationError::UnknownCode {
                                    system: "LOINC",
                                    code: code.clone(),
                                },
                            ));
                        }
                        if !ctx.is_valid_ucum(&unit) {
                            self.errors.push(TypeError::ValidationFailed(
                                ValidationError::InvalidUnit { unit: unit.clone() },
                            ));
                        }
                        if let Some((min, max)) = ctx.reference_range(&code, &unit) {
                            if val < min || val > max {
                                self.errors.push(TypeError::ValidationFailed(
                                    ValidationError::OutOfReferenceRange {
                                        code: code.clone(),
                                        unit: unit.clone(),
                                        min,
                                        max,
                                        actual: val,
                                    },
                                ));
                            }
                        }
                    }
                }
                MediType::Struct(fields)
            }
        };
        // Record in side type table using the expression span
        let span = expr.span();
        self.type_table.insert((span.start, span.end), ty.clone());
        ty
    }

    /// Check a single statement. Updates the environment as needed (e.g., let bindings).
    pub fn check_stmt(&mut self, stmt: &StatementNode) -> Result<(), TypeError> {
        match stmt {
            StatementNode::TypeDecl(decl) => {
                // Build a struct type from declared fields and register it by name
                let mut field_map: std::collections::HashMap<String, MediType> =
                    std::collections::HashMap::new();
                for f in &decl.fields {
                    let ty = self.resolve_annotation_type(&f.type_annotation);
                    field_map.insert(f.name.to_string(), ty);
                }
                self.env
                    .insert(decl.name.name().to_string(), MediType::Struct(field_map));
                Ok(())
            }
            StatementNode::Let(let_stmt) => {
                // Resolve optional annotation
                let annotated_ty = let_stmt
                    .type_annotation
                    .as_ref()
                    .map(|e| self.resolve_annotation_type(e));

                // Check optional initializer
                let init_ty = let_stmt.value.as_ref().map(|expr| self.check_expr(expr));

                // If both present, ensure compatibility
                if let (Some(exp), Some(found)) = (annotated_ty.clone(), init_ty.clone()) {
                    if !found.is_assignable_to(&exp) {
                        return Err(TypeError::TypeMismatch {
                            expected: exp,
                            found,
                        });
                    }
                }

                // Choose binding type: annotated if present, else initializer type, else Unknown
                let bind_ty = annotated_ty
                    .clone()
                    .or(init_ty.clone())
                    .unwrap_or(MediType::Unknown);
                self.env
                    .insert(let_stmt.name.name().to_string(), bind_ty.clone());

                // Record type for the let statement span in the side table
                let s = &let_stmt.span;
                self.type_table.insert((s.start, s.end), bind_ty);
                Ok(())
            }
            StatementNode::Assignment(assign) => {
                let value_ty = self.check_expr(&assign.value);
                match &assign.target {
                    ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                        if let Some(target_ty) = self.env.get(ident.name()) {
                            if value_ty.is_assignable_to(target_ty) {
                                Ok(())
                            } else {
                                Err(TypeError::TypeMismatch {
                                    expected: target_ty.clone(),
                                    found: value_ty,
                                })
                            }
                        } else {
                            Err(TypeError::UnknownIdentifier(ident.name().to_string()))
                        }
                    }
                    ExpressionNode::Member(_) => {
                        // For now, allow member assignment without deep checking
                        Ok(())
                    }
                    _ => Err(TypeError::InvalidAssignmentTarget),
                }
            }
            StatementNode::Expr(expr) => {
                let _ = self.check_expr(expr);
                Ok(())
            }
            StatementNode::Block(block) => {
                // New nested scope
                let parent = self.env.clone();
                let mut child = TypeEnv::with_parent(parent);
                let mut nested = TypeChecker::new(&mut child);
                if let Some(ctx) = self.validation_ctx {
                    nested = nested.with_validation_ctx(ctx);
                }
                for s in &block.statements {
                    nested.check_stmt(s)?;
                }
                // merge nested validation errors
                self.errors.extend(nested.take_errors());
                Ok(())
            }
            StatementNode::If(if_node) => {
                let cond_ty = self.check_expr(&if_node.condition);
                if cond_ty != MediType::Bool {
                    return Err(TypeError::ConditionNotBool(cond_ty));
                }
                // then branch
                let parent = self.env.clone();
                let mut then_env = TypeEnv::with_parent(parent);
                let mut then_ck = TypeChecker::new(&mut then_env);
                if let Some(ctx) = self.validation_ctx {
                    then_ck = then_ck.with_validation_ctx(ctx);
                }
                for s in &if_node.then_branch.statements {
                    then_ck.check_stmt(s)?;
                }
                self.errors.extend(then_ck.take_errors());
                // else branch (if present) - treat generically
                if let Some(else_stmt) = &if_node.else_branch {
                    let parent = self.env.clone();
                    let mut else_env = TypeEnv::with_parent(parent);
                    let mut else_ck = TypeChecker::new(&mut else_env);
                    if let Some(ctx) = self.validation_ctx {
                        else_ck = else_ck.with_validation_ctx(ctx);
                    }
                    else_ck.check_stmt(else_stmt)?;
                    self.errors.extend(else_ck.take_errors());
                }
                Ok(())
            }
            StatementNode::While(while_node) => {
                let cond_ty = self.check_expr(&while_node.condition);
                if cond_ty != MediType::Bool {
                    return Err(TypeError::ConditionNotBool(cond_ty));
                }
                let parent = self.env.clone();
                let mut child = TypeEnv::with_parent(parent);
                let mut ck = TypeChecker::new(&mut child);
                if let Some(ctx) = self.validation_ctx {
                    ck = ck.with_validation_ctx(ctx);
                }
                for s in &while_node.body.statements {
                    ck.check_stmt(s)?;
                }
                self.errors.extend(ck.take_errors());
                Ok(())
            }
            StatementNode::For(for_node) => {
                let it_ty = self.check_expr(&for_node.iterable);
                if let MediType::List(elem) = it_ty {
                    let parent = self.env.clone();
                    let mut child = TypeEnv::with_parent(parent);
                    child.insert(for_node.variable.name().to_string(), *elem);
                    let mut ck = TypeChecker::new(&mut child);
                    if let Some(ctx) = self.validation_ctx {
                        ck = ck.with_validation_ctx(ctx);
                    }
                    for s in &for_node.body.statements {
                        ck.check_stmt(s)?;
                    }
                    self.errors.extend(ck.take_errors());
                    Ok(())
                } else {
                    Err(TypeError::TypeMismatch {
                        expected: MediType::List(Box::new(MediType::Unknown)),
                        found: it_ty,
                    })
                }
            }
            StatementNode::Match(_m) => {
                // Minimal stub
                Ok(())
            }
            StatementNode::Return(ret) => {
                if let Some(expr) = &ret.value {
                    let _ = self.check_expr(expr);
                }
                Ok(())
            }
            StatementNode::Function(fun) => {
                // Build param types
                let mut param_tys = Vec::new();
                for p in &fun.params {
                    let p_ty = p
                        .type_annotation
                        .as_ref()
                        .map(|e| self.resolve_annotation_type(e))
                        .unwrap_or(MediType::Unknown);
                    param_tys.push(p_ty);
                }
                let ret_ty = fun
                    .return_type
                    .as_ref()
                    .map(|e| self.resolve_annotation_type(e))
                    .unwrap_or(MediType::Void);
                // Register the function symbol in current env
                self.env.insert(
                    fun.name.name().to_string(),
                    MediType::Function {
                        params: param_tys.clone(),
                        return_type: Box::new(ret_ty.clone()),
                    },
                );
                // Check body in new scope with param bindings
                let parent = self.env.clone();
                let mut child = TypeEnv::with_parent(parent);
                for (p, ty) in fun.params.iter().zip(param_tys.into_iter()) {
                    child.insert(p.name.name().to_string(), ty);
                }
                let mut ck = TypeChecker::new(&mut child);
                if let Some(ctx) = self.validation_ctx {
                    ck = ck.with_validation_ctx(ctx);
                }
                for s in &fun.body.statements {
                    ck.check_stmt(s)?;
                }
                self.errors.extend(ck.take_errors());
                Ok(())
            }
        }
    }

    /// Check a whole program, returning collected errors.
    pub fn check_program(&mut self, program: &ProgramNode) -> Vec<TypeError> {
        let mut errors = Vec::new();
        for s in &program.statements {
            if let Err(e) = self.check_stmt(s) {
                errors.push(e);
            }
        }
        errors
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeError {
    UnknownIdentifier(String),
    UnknownTypeName(String),
    TypeMismatch {
        expected: MediType,
        found: MediType,
    },
    InvalidAssignmentTarget,
    ConditionNotBool(MediType),
    /// Validation errors coming from UCUM/LOINC/SNOMED checks
    ValidationFailed(ValidationError),
}

impl std::fmt::Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnknownIdentifier(name) => write!(f, "Unknown identifier '{name}'."),
            TypeError::UnknownTypeName(name) => write!(f, "Unknown type name '{name}'."),
            TypeError::TypeMismatch { expected, found } => {
                write!(f, "Type mismatch: expected {expected:?}, found {found:?}.")
            }
            TypeError::InvalidAssignmentTarget => {
                write!(
                    f,
                    "Invalid assignment target; expected identifier or member expression."
                )
            }
            TypeError::ConditionNotBool(found) => {
                write!(f, "Condition must be Bool, found {found:?}.")
            }
            TypeError::ValidationFailed(err) => {
                write!(f, "Validation failed: {err}")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use medic_ast::visit::Span;

    #[test]
    fn let_infers_and_binds() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);
        let stmt = StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name("x"),
            type_annotation: None,
            value: Some(ExpressionNode::Literal(Spanned::new(
                LiteralNode::Int(5),
                Span::default(),
            ))),
            span: Span::default(),
        }));
        assert!(tc.check_stmt(&stmt).is_ok());
        assert_eq!(tc.env.get("x"), Some(&MediType::Int));
    }

    #[test]
    fn if_requires_bool_condition() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);
        let stmt = StatementNode::If(Box::new(IfNode {
            condition: ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), Span::default())),
            then_branch: BlockNode {
                statements: vec![].into(),
                span: Span::default(),
            },
            else_branch: None,
            span: Span::default(),
        }));
        let err = tc.check_stmt(&stmt).unwrap_err();
        assert!(matches!(err, TypeError::ConditionNotBool(MediType::Int)));
    }

    #[test]
    fn function_registration_and_body_scope() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);
        let fun = StatementNode::Function(Box::new(FunctionNode {
            name: IdentifierNode::from_str_name("add"),
            params: vec![
                ParameterNode {
                    name: IdentifierNode::from_str_name("a"),
                    type_annotation: Some(ExpressionNode::Identifier(Spanned::new(
                        IdentifierNode::from_str_name("Int"),
                        Span::default(),
                    ))),
                    span: Span::default(),
                },
                ParameterNode {
                    name: IdentifierNode::from_str_name("b"),
                    type_annotation: Some(ExpressionNode::Identifier(Spanned::new(
                        IdentifierNode::from_str_name("Int"),
                        Span::default(),
                    ))),
                    span: Span::default(),
                },
            ]
            .into(),
            return_type: Some(ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("Int"),
                Span::default(),
            ))),
            body: BlockNode {
                statements: vec![StatementNode::Let(Box::new(LetStatementNode {
                    name: IdentifierNode::from_str_name("x"),
                    type_annotation: None,
                    value: Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(1),
                        Span::default(),
                    ))),
                    span: Span::default(),
                }))]
                .into(),
                span: Span::default(),
            },
            span: Span::default(),
        }));
        assert!(tc.check_stmt(&fun).is_ok());
        let ty = tc.env.get("add").cloned();
        match ty {
            Some(MediType::Function { params, .. }) => {
                assert_eq!(params, vec![MediType::Int, MediType::Int]);
            }
            _ => panic!("function type not found or incorrect"),
        }
    }

    #[test]
    fn primitive_ops_and_comparisons() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // 1 + 2 -> Int
        let expr_int_add = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Int(1),
                    Span {
                        start: 0,
                        end: 1,
                        line: 1,
                        column: 1,
                    },
                )),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Int(2),
                    Span {
                        start: 2,
                        end: 3,
                        line: 1,
                        column: 3,
                    },
                )),
            }),
            Span {
                start: 0,
                end: 3,
                line: 1,
                column: 1,
            },
        ));
        assert_eq!(tc.check_expr(&expr_int_add), MediType::Int);

        // 1 + 2.0 -> Float (numeric promotion)
        let expr_mix_add = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Int(1),
                    Span {
                        start: 0,
                        end: 1,
                        line: 1,
                        column: 1,
                    },
                )),
                operator: BinaryOperator::Add,
                right: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Float(2.0),
                    Span {
                        start: 2,
                        end: 5,
                        line: 1,
                        column: 3,
                    },
                )),
            }),
            Span {
                start: 0,
                end: 5,
                line: 1,
                column: 1,
            },
        ));
        assert_eq!(tc.check_expr(&expr_mix_add), MediType::Float);

        // 1 < 2 -> Bool
        let expr_lt = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Int(1),
                    Span {
                        start: 0,
                        end: 1,
                        line: 1,
                        column: 1,
                    },
                )),
                operator: BinaryOperator::Lt,
                right: ExpressionNode::Literal(Spanned::new(
                    LiteralNode::Int(2),
                    Span {
                        start: 4,
                        end: 5,
                        line: 1,
                        column: 5,
                    },
                )),
            }),
            Span {
                start: 0,
                end: 5,
                line: 1,
                column: 1,
            },
        ));
        assert_eq!(tc.check_expr(&expr_lt), MediType::Bool);
    }

    #[test]
    fn type_table_records_computed_types() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // Build (1 + 2) with distinct spans for children and parent
        let left = ExpressionNode::Literal(Spanned::new(
            LiteralNode::Int(1),
            Span {
                start: 10,
                end: 11,
                line: 1,
                column: 11,
            },
        ));
        let right = ExpressionNode::Literal(Spanned::new(
            LiteralNode::Int(2),
            Span {
                start: 14,
                end: 15,
                line: 1,
                column: 15,
            },
        ));
        let bin = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left,
                operator: BinaryOperator::Add,
                right,
            }),
            Span {
                start: 10,
                end: 15,
                line: 1,
                column: 11,
            },
        ));

        let ty = tc.check_expr(&bin);
        assert_eq!(ty, MediType::Int);

        // Parent span should be recorded as Int
        let parent_span = Span {
            start: 10,
            end: 15,
            line: 1,
            column: 11,
        };
        assert_eq!(tc.get_type_at_span(&parent_span), Some(&MediType::Int));

        // Children spans should also be present as Int
        let left_span = Span {
            start: 10,
            end: 11,
            line: 1,
            column: 11,
        };
        let right_span = Span {
            start: 14,
            end: 15,
            line: 1,
            column: 15,
        };
        assert_eq!(tc.get_type_at_span(&left_span), Some(&MediType::Int));
        assert_eq!(tc.get_type_at_span(&right_span), Some(&MediType::Int));
    }

    #[test]
    fn let_with_matching_annotation_binds_annotated_type() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // let x: Int = 1;
        let span = Span {
            start: 0,
            end: 10,
            line: 1,
            column: 1,
        };
        let ann =
            ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("Int"), span));
        let val = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), span));
        let stmt = StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name("x"),
            type_annotation: Some(ann),
            value: Some(val),
            span,
        }));

        assert!(tc.check_stmt(&stmt).is_ok());
        assert_eq!(tc.env.get("x"), Some(&MediType::Int));
        assert_eq!(tc.get_type_at_span(&span), Some(&MediType::Int));
    }

    #[test]
    fn let_with_mismatching_annotation_errors() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // let x: String = 1; -> mismatch
        let span = Span {
            start: 0,
            end: 10,
            line: 1,
            column: 1,
        };
        let ann =
            ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("String"), span));
        let val = ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), span));
        let stmt = StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name("x"),
            type_annotation: Some(ann),
            value: Some(val),
            span,
        }));

        let err = tc.check_stmt(&stmt).unwrap_err();
        match err {
            TypeError::TypeMismatch { expected, found } => {
                assert_eq!(expected, MediType::String);
                assert_eq!(found, MediType::Int);
            }
            _ => panic!("expected TypeMismatch error, got {err:?}"),
        }
    }

    #[test]
    fn let_without_annotation_infers_from_initializer() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // let y = 2.0;
        let span = Span {
            start: 5,
            end: 9,
            line: 1,
            column: 6,
        };
        let val = ExpressionNode::Literal(Spanned::new(LiteralNode::Float(2.0), span));
        let stmt = StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name("y"),
            type_annotation: None,
            value: Some(val),
            span,
        }));

        assert!(tc.check_stmt(&stmt).is_ok());
        assert_eq!(tc.env.get("y"), Some(&MediType::Float));
        assert_eq!(tc.get_type_at_span(&span), Some(&MediType::Float));
    }

    #[test]
    fn call_type_checking_and_type_table() {
        let mut env = TypeEnv::with_prelude();
        // Register a function type: add(Int, Int) -> Int
        env.insert(
            "add".to_string(),
            MediType::Function {
                params: vec![MediType::Int, MediType::Int],
                return_type: Box::new(MediType::Int),
            },
        );
        let mut tc = TypeChecker::new(&mut env);

        // Build add(1, 2)
        let callee_ident = IdentifierNode::from_str_name("add");
        let callee_span = Span {
            start: 0,
            end: 3,
            line: 1,
            column: 1,
        };
        let arg1_span = Span {
            start: 4,
            end: 5,
            line: 1,
            column: 5,
        };
        let arg2_span = Span {
            start: 7,
            end: 8,
            line: 1,
            column: 8,
        };
        let call_span = Span {
            start: 0,
            end: 9,
            line: 1,
            column: 1,
        };

        let call_expr = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ExpressionNode::Identifier(Spanned::new(callee_ident.clone(), callee_span)),
                arguments: vec![
                    ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), arg1_span)),
                    ExpressionNode::Literal(Spanned::new(LiteralNode::Int(2), arg2_span)),
                ]
                .into(),
            }),
            call_span,
        ));

        // Type should resolve to Int
        let ty = tc.check_expr(&call_expr);
        assert_eq!(ty, MediType::Int);

        // Type table should record parent, callee, and arg types
        assert_eq!(tc.get_type_at_span(&call_span), Some(&MediType::Int));
        assert_eq!(
            tc.get_type_at_span(&callee_span),
            Some(&MediType::Function {
                params: vec![MediType::Int, MediType::Int],
                return_type: Box::new(MediType::Int)
            })
        );
        assert_eq!(tc.get_type_at_span(&arg1_span), Some(&MediType::Int));
        assert_eq!(tc.get_type_at_span(&arg2_span), Some(&MediType::Int));
    }
}
