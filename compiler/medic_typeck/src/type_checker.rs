// Type checker for Medic language in Rust
// This is a basic structure; expand with more rules as needed

use crate::units;
use medic_ast::ast::*;
use medic_ast::visit::Span;
use medic_env::env::{SinkKind, TypeEnv};
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
    /// Side privacy table: maps (start,end) of expression span to its inferred privacy label
    privacy_table: HashMap<(usize, usize), PrivacyAnnotation>,
    /// Side unit metadata: maps (start,end) span of an expression to its unit symbol (e.g., "mg")
    quantity_units: HashMap<(usize, usize), String>,
    /// Optional per-function parameter unit expectations: (function_name, param_index) -> unit
    param_unit_expectations: HashMap<(String, usize), String>,
    /// Optional per-function return unit expectation: function_name -> unit
    return_unit_expectations: HashMap<String, String>,
}

#[cfg(test)]
#[allow(clippy::vec_init_then_push)]
mod unit_tests {
    use super::*;

    #[test]
    fn unit_conversion_known_factor_records_unit_and_returns_float() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // Build (5 mg) -> g
        let span_qty = Span {
            start: 0,
            end: 3,
            line: 1,
            column: 1,
        };
        let qty = ExpressionNode::Quantity(Spanned::new(
            Box::new(QuantityLiteralNode {
                value: LiteralNode::Int(5),
                unit: IdentifierNode::from_str_name("mg"),
            }),
            span_qty,
        ));
        let span_rhs = Span {
            start: 7,
            end: 8,
            line: 1,
            column: 8,
        };
        let rhs =
            ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("g"), span_rhs));
        let span_parent = Span {
            start: 0,
            end: 8,
            line: 1,
            column: 1,
        };
        let expr = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: qty,
                operator: BinaryOperator::UnitConversion,
                right: rhs,
            }),
            span_parent,
        ));

        let ty = tc.check_expr(&expr);
        assert_eq!(ty, MediType::Quantity(Box::new(MediType::Float)));
        // Unit metadata should record target unit at parent span
        assert_eq!(tc.get_unit_at_span(&span_parent), Some(&"g".to_string()));
        // No errors
        assert!(tc.errors().is_empty());
    }

    #[test]
    fn collect_generic_specializations_for_literal_calls() {
        use medic_ast::visit::Span;
        // env: f: fn(T) -> T
        let mut env = TypeEnv::with_prelude();
        env.insert(
            "f".to_string(),
            MediType::Function {
                params: vec![MediType::TypeVar("T".to_string())],
                return_type: Box::new(MediType::TypeVar("T".to_string())),
            },
        );

        // Build AST: fn f(x: T) -> T { x }
        let f_fn = StatementNode::Function(Box::new(FunctionNode {
            name: IdentifierNode::from_str_name("f"),
            params: {
                let mut p: NodeList<ParameterNode> = NodeList::new();
                p.extend([ParameterNode {
                    name: IdentifierNode::from_str_name("x"),
                    type_annotation: Some(ExpressionNode::Identifier(Spanned::new(
                        IdentifierNode::from_str_name("T"),
                        Span {
                            start: 0,
                            end: 1,
                            line: 1,
                            column: 1,
                        },
                    ))),
                    span: Span {
                        start: 0,
                        end: 0,
                        line: 1,
                        column: 1,
                    },
                }]);
                p
            },
            return_type: Some(ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("T"),
                Span {
                    start: 0,
                    end: 1,
                    line: 1,
                    column: 1,
                },
            ))),
            body: BlockNode {
                statements: {
                    let mut s: NodeList<StatementNode> = NodeList::new();
                    s.extend([StatementNode::Return(Box::new(ReturnNode {
                        value: Some(ExpressionNode::Identifier(Spanned::new(
                            IdentifierNode::from_str_name("x"),
                            Span {
                                start: 0,
                                end: 1,
                                line: 1,
                                column: 1,
                            },
                        ))),
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 1,
                            column: 1,
                        },
                    }))]);
                    s
                },
                span: Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                },
            },
            span: Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        }));

        // fn test() { let a = f(1); let b = f(1.0); }
        let test_fn = StatementNode::Function(Box::new(FunctionNode {
            name: IdentifierNode::from_str_name("test"),
            params: NodeList::new(),
            return_type: None,
            body: BlockNode {
                statements: [
                    StatementNode::Let(Box::new(LetStatementNode {
                        name: IdentifierNode::from_str_name("a"),
                        type_annotation: None,
                        value: Some(ExpressionNode::Call(Spanned::new(
                            Box::new(CallExpressionNode {
                                callee: ExpressionNode::Identifier(Spanned::new(
                                    IdentifierNode::from_str_name("f"),
                                    Span {
                                        start: 0,
                                        end: 1,
                                        line: 1,
                                        column: 1,
                                    },
                                )),
                                arguments: {
                                    let mut args: NodeList<ExpressionNode> = NodeList::new();
                                    args.extend([ExpressionNode::Literal(Spanned::new(
                                        LiteralNode::Int(1),
                                        Span {
                                            start: 2,
                                            end: 3,
                                            line: 1,
                                            column: 3,
                                        },
                                    ))]);
                                    args
                                },
                            }),
                            Span {
                                start: 0,
                                end: 3,
                                line: 1,
                                column: 1,
                            },
                        ))),
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 1,
                            column: 1,
                        },
                    })),
                    StatementNode::Let(Box::new(LetStatementNode {
                        name: IdentifierNode::from_str_name("b"),
                        type_annotation: None,
                        value: Some(ExpressionNode::Call(Spanned::new(
                            Box::new(CallExpressionNode {
                                callee: ExpressionNode::Identifier(Spanned::new(
                                    IdentifierNode::from_str_name("f"),
                                    Span {
                                        start: 0,
                                        end: 1,
                                        line: 1,
                                        column: 1,
                                    },
                                )),
                                arguments: {
                                    let mut args: NodeList<ExpressionNode> = NodeList::new();
                                    args.extend([ExpressionNode::Literal(Spanned::new(
                                        LiteralNode::Float(1.0),
                                        Span {
                                            start: 2,
                                            end: 5,
                                            line: 1,
                                            column: 3,
                                        },
                                    ))]);
                                    args
                                },
                            }),
                            Span {
                                start: 0,
                                end: 5,
                                line: 1,
                                column: 1,
                            },
                        ))),
                        span: Span {
                            start: 0,
                            end: 0,
                            line: 1,
                            column: 1,
                        },
                    })),
                ]
                .into_iter()
                .collect(),
                span: Span {
                    start: 0,
                    end: 0,
                    line: 1,
                    column: 1,
                },
            },
            span: Span {
                start: 0,
                end: 0,
                line: 1,
                column: 1,
            },
        }));

        let program = ProgramNode {
            statements: {
                let mut nl: NodeList<StatementNode> = NodeList::new();
                nl.extend([f_fn, test_fn]);
                nl
            },
        };

        let tc = TypeChecker::new(&mut env);
        let specs = tc.collect_function_specializations(&program);
        // Expect two concrete specializations: (f, [Int], Int) and (f, [Float], Float)
        assert!(specs.contains(&("f".to_string(), vec![MediType::Int], MediType::Int,)));
        assert!(specs.contains(&("f".to_string(), vec![MediType::Float], MediType::Float,)));
    }

    // Removed duplicate collect_function_specializations helper from test module; use the impl method below.

    // Note: expectation setters are implemented on TypeChecker impl for test use

    #[test]
    fn unit_conversion_unknown_unit_produces_error() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // (5 mg) -> zzz (unknown)
        let span_qty = Span {
            start: 0,
            end: 3,
            line: 1,
            column: 1,
        };
        let qty = ExpressionNode::Quantity(Spanned::new(
            Box::new(QuantityLiteralNode {
                value: LiteralNode::Int(5),
                unit: IdentifierNode::from_str_name("mg"),
            }),
            span_qty,
        ));
        let span_rhs = Span {
            start: 7,
            end: 10,
            line: 1,
            column: 8,
        };
        let rhs = ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("zzz"),
            span_rhs,
        ));
        let span_parent = Span {
            start: 0,
            end: 10,
            line: 1,
            column: 1,
        };
        let expr = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: qty,
                operator: BinaryOperator::UnitConversion,
                right: rhs,
            }),
            span_parent,
        ));

        let _ = tc.check_expr(&expr);
        let errs = tc.take_errors();
        assert!(!errs.is_empty());
    }

    #[test]
    fn unit_conversion_dimension_mismatch_produces_error() {
        let mut env = TypeEnv::with_prelude();
        let mut tc = TypeChecker::new(&mut env);

        // (5 cm) -> s (Length -> Time)
        let span_qty = Span {
            start: 0,
            end: 3,
            line: 1,
            column: 1,
        };
        let qty = ExpressionNode::Quantity(Spanned::new(
            Box::new(QuantityLiteralNode {
                value: LiteralNode::Int(5),
                unit: IdentifierNode::from_str_name("cm"),
            }),
            span_qty,
        ));
        let span_rhs = Span {
            start: 7,
            end: 8,
            line: 1,
            column: 8,
        };
        let rhs =
            ExpressionNode::Identifier(Spanned::new(IdentifierNode::from_str_name("s"), span_rhs));
        let span_parent = Span {
            start: 0,
            end: 8,
            line: 1,
            column: 1,
        };
        let expr = ExpressionNode::Binary(Spanned::new(
            Box::new(BinaryExpressionNode {
                left: qty,
                operator: BinaryOperator::UnitConversion,
                right: rhs,
            }),
            span_parent,
        ));

        let _ = tc.check_expr(&expr);
        let errs = tc.take_errors();
        assert!(!errs.is_empty());
    }

    #[test]
    fn call_param_unit_expectation_success_and_return_unit_recorded() {
        let mut env = TypeEnv::with_prelude();
        // Register a function expecting a quantity param and returning Float
        env.insert(
            "dose_norm".to_string(),
            MediType::Function {
                params: vec![MediType::Quantity(Box::new(MediType::Float))],
                return_type: Box::new(MediType::Float),
            },
        );
        let mut tc = TypeChecker::new(&mut env);
        tc.set_param_unit_expectation("dose_norm", 0, "mg");
        tc.set_return_unit_expectation("dose_norm", "mg");

        // Build dose_norm(5 mg)
        let callee_span = Span {
            start: 0,
            end: 8,
            line: 1,
            column: 1,
        };
        let callee = ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("dose_norm"),
            callee_span,
        ));
        let arg_span = Span {
            start: 9,
            end: 14,
            line: 1,
            column: 10,
        };
        let arg = ExpressionNode::Quantity(Spanned::new(
            Box::new(QuantityLiteralNode {
                value: LiteralNode::Int(5),
                unit: IdentifierNode::from_str_name("mg"),
            }),
            arg_span,
        ));
        let call_span = Span {
            start: 0,
            end: 14,
            line: 1,
            column: 1,
        };
        let call = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee,
                arguments: {
                    let mut a: NodeList<ExpressionNode> = NodeList::new();
                    a.extend([arg]);
                    a
                },
            }),
            call_span,
        ));

        let ty = tc.check_expr(&call);
        assert_eq!(ty, MediType::Quantity(Box::new(MediType::Float)));
        // Return unit expectation should be recorded at call span
        assert_eq!(tc.get_unit_at_span(&call_span), Some(&"mg".to_string()));
        assert!(tc.errors().is_empty());
    }

    #[test]
    fn call_param_unit_expectation_dimension_mismatch_errors() {
        let mut env = TypeEnv::with_prelude();
        env.insert(
            "f".to_string(),
            MediType::Function {
                params: vec![MediType::Quantity(Box::new(MediType::Float))],
                return_type: Box::new(MediType::Float),
            },
        );
        let mut tc = TypeChecker::new(&mut env);
        // Expect time unit, but provide a length quantity
        tc.set_param_unit_expectation("f", 0, "s");

        let callee = ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("f"),
            Span {
                start: 0,
                end: 1,
                line: 1,
                column: 1,
            },
        ));
        let arg = ExpressionNode::Quantity(Spanned::new(
            Box::new(QuantityLiteralNode {
                value: LiteralNode::Int(5),
                unit: IdentifierNode::from_str_name("cm"),
            }),
            Span {
                start: 3,
                end: 6,
                line: 1,
                column: 4,
            },
        ));
        let call_span = Span {
            start: 0,
            end: 6,
            line: 1,
            column: 1,
        };
        let call = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee,
                arguments: {
                    let mut a: NodeList<ExpressionNode> = NodeList::new();
                    a.extend([arg]);
                    a
                },
            }),
            call_span,
        ));

        let _ = tc.check_expr(&call);
        let errs = tc.take_errors();
        assert!(!errs.is_empty());
    }

    #[test]
    fn call_param_unit_expectation_missing_unit_errors() {
        let mut env = TypeEnv::with_prelude();
        env.insert(
            "g".to_string(),
            MediType::Function {
                params: vec![MediType::Quantity(Box::new(MediType::Float))],
                return_type: Box::new(MediType::Float),
            },
        );
        let mut tc = TypeChecker::new(&mut env);
        tc.set_param_unit_expectation("g", 0, "mg");

        let callee = ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name("g"),
            Span {
                start: 0,
                end: 1,
                line: 1,
                column: 1,
            },
        ));
        // Arg is numeric literal without unit
        let arg = ExpressionNode::Literal(Spanned::new(
            LiteralNode::Float(5.0),
            Span {
                start: 3,
                end: 4,
                line: 1,
                column: 4,
            },
        ));
        let call_span = Span {
            start: 0,
            end: 4,
            line: 1,
            column: 1,
        };
        let call = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee,
                arguments: {
                    let mut a: NodeList<ExpressionNode> = NodeList::new();
                    a.extend([arg]);
                    a
                },
            }),
            call_span,
        ));

        let _ = tc.check_expr(&call);
        let errs = tc.take_errors();
        assert!(!errs.is_empty());
    }
}

impl<'a> TypeChecker<'a> {
    pub fn new(env: &'a mut TypeEnv) -> Self {
        TypeChecker {
            env,
            type_table: HashMap::new(),
            validation_ctx: None,
            errors: Vec::new(),
            privacy_table: HashMap::new(),
            quantity_units: HashMap::new(),
            param_unit_expectations: HashMap::new(),
            return_unit_expectations: HashMap::new(),
        }
    }

    /// Set expected unit for a function parameter (by function name and param index).
    pub fn set_param_unit_expectation(&mut self, func: &str, index: usize, unit: &str) {
        self.param_unit_expectations
            .insert((func.to_string(), index), unit.to_string());
    }

    /// Set expected unit for a function return value (by function name).
    pub fn set_return_unit_expectation(&mut self, func: &str, unit: &str) {
        self.return_unit_expectations
            .insert(func.to_string(), unit.to_string());
    }

    /// Provide a validation context for UCUM/LOINC/SNOMED-aware checks.
    pub fn with_validation_ctx(mut self, ctx: &'a ValidationCtx) -> Self {
        self.validation_ctx = Some(ctx);
        self
    }

    /// Drain and return collected errors
    pub fn take_errors(&mut self) -> Vec<TypeError> {
        std::mem::take(&mut self.errors)
    }

    /// View collected errors without draining (used by compliance checker).
    pub fn errors(&self) -> &Vec<TypeError> {
        &self.errors
    }

    /// Access the ambient type environment (read-only).
    pub fn env(&self) -> &TypeEnv {
        self.env
    }

    /// Look up the inferred privacy label for a given span, if present.
    pub fn privacy_label_for_span(&self, span: &Span) -> Option<PrivacyAnnotation> {
        self.privacy_table.get(&(span.start, span.end)).copied()
    }

    /// Returns an immutable view of the side type table.
    pub fn type_table(&self) -> &HashMap<(usize, usize), MediType> {
        &self.type_table
    }

    /// Looks up a computed type by a `Span` key.
    pub fn get_type_at_span(&self, span: &Span) -> Option<&MediType> {
        self.type_table.get(&(span.start, span.end))
    }

    /// Looks up a computed privacy label by a `Span` key.
    pub fn get_privacy_at_span(&self, span: &Span) -> Option<&PrivacyAnnotation> {
        self.privacy_table.get(&(span.start, span.end))
    }

    /// Returns an immutable view of the side privacy table.
    pub fn privacy_table_map(&self) -> &HashMap<(usize, usize), PrivacyAnnotation> {
        &self.privacy_table
    }

    /// Looks up a recorded unit string by a `Span` key.
    pub fn get_unit_at_span(&self, span: &Span) -> Option<&String> {
        self.quantity_units.get(&(span.start, span.end))
    }

    /// Records a unit for a given expression span.
    fn set_unit_at_span(&mut self, span: &Span, unit: &str) {
        self.quantity_units
            .insert((span.start, span.end), unit.to_string());
    }

    /// Walk the program to collect concrete function specializations derived from generic function types.
    /// For each call f(args...), if env has f: Function with type variables (TypeVar), attempt to unify
    /// formal params with actual argument types and produce concrete param/return MediTypes.
    /// Returns unique list of (function_name, concrete_params, concrete_return).
    pub fn collect_function_specializations(
        &self,
        program: &ProgramNode,
    ) -> Vec<(String, Vec<MediType>, MediType)> {
        use std::collections::HashMap;

        fn apply_subs(t: &MediType, subs: &HashMap<String, MediType>) -> MediType {
            use MediType as MT;
            match t {
                MT::TypeVar(n) => subs.get(n).cloned().unwrap_or(MT::Unknown),
                MT::Range(inner) => MT::Range(Box::new(apply_subs(inner, subs))),
                MT::Quantity(inner) => MT::Quantity(Box::new(apply_subs(inner, subs))),
                MT::List(inner) => MT::List(Box::new(apply_subs(inner, subs))),
                MT::Struct(fields) => {
                    let mut m = HashMap::new();
                    for (k, v) in fields.iter() {
                        m.insert(k.clone(), apply_subs(v, subs));
                    }
                    MT::Struct(m)
                }
                MT::Record(fields) => MT::Record(
                    fields
                        .iter()
                        .map(|(k, v)| (k.clone(), apply_subs(v, subs)))
                        .collect(),
                ),
                MT::Function {
                    params,
                    return_type,
                } => MT::Function {
                    params: params.iter().map(|p| apply_subs(p, subs)).collect(),
                    return_type: Box::new(apply_subs(return_type, subs)),
                },
                _ => t.clone(),
            }
        }

        fn unify(
            formal: &MediType,
            actual: &MediType,
            subs: &mut HashMap<String, MediType>,
        ) -> bool {
            use MediType as MT;
            match (formal, actual) {
                (MT::TypeVar(n), a) => {
                    if let Some(prev) = subs.get(n) {
                        prev == a
                    } else {
                        subs.insert(n.clone(), a.clone());
                        true
                    }
                }
                (MT::Range(f), MT::Range(a)) | (MT::Quantity(f), MT::Quantity(a)) => {
                    unify(f, a, subs)
                }
                (MT::List(f), MT::List(a)) => unify(f, a, subs),
                (MT::Struct(fs), MT::Struct(as_)) => {
                    if fs.len() != as_.len() {
                        return false;
                    }
                    for (k, fv) in fs.iter() {
                        if let Some(av) = as_.get(k) {
                            if !unify(fv, av, subs) {
                                return false;
                            }
                        } else {
                            return false;
                        }
                    }
                    true
                }
                (MT::Record(fr), MT::Record(ar)) => {
                    if fr.len() != ar.len() {
                        return false;
                    }
                    for ((fk, fv), (ak, av)) in fr.iter().zip(ar.iter()) {
                        if fk != ak || !unify(fv, av, subs) {
                            return false;
                        }
                    }
                    true
                }
                (f, a) => f == a,
            }
        }

        struct Collector<'a> {
            tc: &'a TypeChecker<'a>,
            specs: Vec<(String, Vec<MediType>, MediType)>,
        }
        impl<'a> Collector<'a> {
            fn simple_infer_expr_type(&self, e: &ExpressionNode) -> Option<MediType> {
                match e {
                    ExpressionNode::Literal(Spanned { node: lit, .. }) => match lit {
                        LiteralNode::Int(_) => Some(MediType::Int),
                        LiteralNode::Float(_) => Some(MediType::Float),
                        LiteralNode::Bool(_) => Some(MediType::Bool),
                        LiteralNode::String(_) => Some(MediType::String),
                    },
                    _ => None,
                }
            }
        }
        impl<'a> Collector<'a> {
            fn visit_expr(&mut self, e: &ExpressionNode) {
                match e {
                    ExpressionNode::Call(Spanned { node: call, .. }) => {
                        if let Some(name) = TypeChecker::extract_identifier_name(&call.callee) {
                            if let Some(MediType::Function {
                                params,
                                return_type,
                            }) = self.tc.env.get(&name)
                            {
                                let is_generic =
                                    params.iter().any(|p| matches!(p, MediType::TypeVar(_)))
                                        || matches!(return_type.as_ref(), MediType::TypeVar(_));
                                if is_generic && params.len() == call.arguments.len() {
                                    let mut subs = HashMap::new();
                                    let mut ok = true;
                                    for (pi, arg) in call.arguments.iter().enumerate() {
                                        let at = self
                                            .simple_infer_expr_type(arg)
                                            .or_else(|| {
                                                self.tc.get_type_at_span(arg.span()).cloned()
                                            })
                                            .unwrap_or(MediType::Unknown);
                                        if !unify(&params[pi], &at, &mut subs) {
                                            ok = false;
                                            break;
                                        }
                                    }
                                    if ok {
                                        let concr_params: Vec<MediType> =
                                            params.iter().map(|p| apply_subs(p, &subs)).collect();
                                        let concr_ret = apply_subs(return_type, &subs);
                                        let item = (name, concr_params, concr_ret);
                                        if !self.specs.iter().any(|x| x == &item) {
                                            self.specs.push(item);
                                        }
                                    }
                                }
                            }
                        }
                        self.visit_expr(&call.callee);
                        for a in &call.arguments {
                            self.visit_expr(a);
                        }
                    }
                    ExpressionNode::Binary(Spanned { node: b, .. }) => {
                        self.visit_expr(&b.left);
                        self.visit_expr(&b.right);
                    }
                    ExpressionNode::Member(Spanned { node: m, .. }) => {
                        self.visit_expr(&m.object);
                    }
                    ExpressionNode::Index(Spanned { node: ix, .. }) => {
                        self.visit_expr(&ix.object);
                        self.visit_expr(&ix.index);
                    }
                    ExpressionNode::Array(Spanned { node: arr, .. }) => {
                        for el in &arr.elements {
                            self.visit_expr(el);
                        }
                    }
                    ExpressionNode::Struct(Spanned { node: s, .. }) => {
                        for f in &s.fields {
                            self.visit_expr(&f.value);
                        }
                    }
                    ExpressionNode::Statement(Spanned { node: stmt, .. }) => {
                        self.visit_stmt(stmt);
                    }
                    _ => {}
                }
            }
            fn visit_stmt(&mut self, s: &StatementNode) {
                match s {
                    StatementNode::Expr(e) => self.visit_expr(e),
                    StatementNode::Let(ln) => {
                        if let Some(init) = &ln.value {
                            self.visit_expr(init);
                        }
                    }
                    StatementNode::Return(rn) => {
                        if let Some(v) = &rn.value {
                            self.visit_expr(v);
                        }
                    }
                    StatementNode::Function(fnnode) => {
                        for st in &fnnode.body.statements {
                            self.visit_stmt(st);
                        }
                    }
                    _ => {}
                }
            }
        }

        let mut coll = Collector {
            tc: self,
            specs: Vec::new(),
        };
        for st in &program.statements {
            coll.visit_stmt(st);
        }
        coll.specs.sort_by(|a, b| a.0.cmp(&b.0));
        coll.specs
    }

    /// Attempts to extract a quantity unit from an expression (Quantity literal or implicit mul like `5 mg`).
    fn extract_quantity_unit(expr: &ExpressionNode) -> Option<String> {
        match expr {
            ExpressionNode::Quantity(Spanned { node: q, .. }) => Some(q.unit.name().to_string()),
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                if let BinaryOperator::Mul = bin.operator {
                    match (&bin.left, &bin.right) {
                        (
                            ExpressionNode::Literal(Spanned {
                                node: LiteralNode::Int(_),
                                ..
                            })
                            | ExpressionNode::Literal(Spanned {
                                node: LiteralNode::Float(_),
                                ..
                            }),
                            ExpressionNode::Identifier(Spanned { node: id, .. }),
                        ) => Some(id.name().to_string()),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    #[cfg(feature = "quantity_ir")]
    /// Attempts to extract a DimVec for an expression when it denotes a quantity with a unit.
    fn extract_dimvec(expr: &ExpressionNode) -> Option<units::DimVec> {
        let u = Self::extract_quantity_unit(expr)?;
        let info = units::lookup_unit(&u)?;
        Some(units::dimvec_of(info.dim))
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

    /// Resolve a privacy annotation from an identifier type annotation, e.g., `: PHI`.
    fn resolve_privacy_annotation(ty_expr: &ExpressionNode) -> Option<PrivacyAnnotation> {
        if let ExpressionNode::Identifier(Spanned { node: ident, .. }) = ty_expr {
            match ident.name() {
                "PHI" | "Phi" | "phi" => Some(PrivacyAnnotation::PHI),
                "Pseudonymized" | "Pseudonymised" | "Pseudo" | "pseudonymized" => {
                    Some(PrivacyAnnotation::Pseudonymized)
                }
                "Anonymized" | "Anon" | "anon" | "Deidentified" => {
                    Some(PrivacyAnnotation::Anonymized)
                }
                "Authorized" | "Auth" | "authorized" => Some(PrivacyAnnotation::Authorized),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Returns whether information can legally flow from src to dst privacy label.
    fn privacy_flow_allowed(src: PrivacyAnnotation, dst: PrivacyAnnotation) -> bool {
        use PrivacyAnnotation::*;
        match (src, dst) {
            // Anonymized can flow anywhere
            (Anonymized, _) => true,
            // Pseudonymized can flow to Pseudonymized or PHI; cannot implicitly downgrade to Anonymized
            (Pseudonymized, Pseudonymized) => true,
            (Pseudonymized, PHI) => true,
            (Pseudonymized, Authorized) => false,
            (Pseudonymized, Anonymized) => false,
            // Authorized can flow to Authorized, and remain Authorized; cannot downgrade to Anonymized
            (Authorized, Authorized) => true,
            (Authorized, Anonymized) => false,
            (Authorized, Pseudonymized) => false,
            (Authorized, PHI) => true, // becoming stricter is fine
            // AuthorizedFor(kind): can flow to same AuthorizedFor(kind) or broader Authorized; cannot flow to different AuthorizedFor or downgrade to Anonymized
            (PrivacyAnnotation::AuthorizedFor(a), PrivacyAnnotation::AuthorizedFor(b)) => a == b,
            (PrivacyAnnotation::AuthorizedFor(_), Authorized) => true,
            (PrivacyAnnotation::AuthorizedFor(_), Anonymized) => false,
            (PrivacyAnnotation::AuthorizedFor(_), Pseudonymized) => false,
            (PrivacyAnnotation::AuthorizedFor(_), PHI) => true,
            // Flows to AuthorizedFor(dest): only allowed if source is AuthorizedFor(same) or Authorized (broader)
            (Pseudonymized, PrivacyAnnotation::AuthorizedFor(_)) => false,
            (Authorized, PrivacyAnnotation::AuthorizedFor(_)) => true,
            (PHI, PrivacyAnnotation::AuthorizedFor(_)) => false,
            // PHI can flow only to PHI; not to Authorized or Anonymized without explicit authorization context
            (PHI, PHI) => true,
            (PHI, Authorized) => false,
            (PHI, Anonymized) => false,
            (PHI, Pseudonymized) => false,
        }
    }

    /// Infer privacy label of an expression and record it.
    fn check_expr_privacy(&mut self, expr: &ExpressionNode) -> PrivacyAnnotation {
        use PrivacyAnnotation::*;
        let label = match expr {
            ExpressionNode::Identifier(Spanned { node: name, .. }) => {
                self.env.get_privacy(name.name()).unwrap_or(Anonymized)
            }
            ExpressionNode::Literal(_) => Anonymized,
            ExpressionNode::Quantity(_) => Anonymized,
            ExpressionNode::IcdCode(_)
            | ExpressionNode::CptCode(_)
            | ExpressionNode::SnomedCode(_) => PHI,
            ExpressionNode::Member(Spanned { node: mem, .. }) => {
                self.check_expr_privacy(&mem.object)
            }
            ExpressionNode::Index(Spanned { node: idx, .. }) => {
                // Propagate privacy from the object being indexed
                self.check_expr_privacy(&idx.object)
            }
            ExpressionNode::Binary(Spanned { node: bin, .. }) => {
                let l = self.check_expr_privacy(&bin.left);
                let r = self.check_expr_privacy(&bin.right);
                l.join(r)
            }
            ExpressionNode::Call(Spanned { node: call, .. }) => {
                // Recognize callee name for special handling
                let callee_name = Self::extract_identifier_name(&call.callee);

                // Conservative: start with join of callee and args
                let mut acc = self.check_expr_privacy(&call.callee);
                for a in &call.arguments {
                    acc = acc.join(self.check_expr_privacy(a));
                }

                // Determine de-identification via env metadata first, fallback to known names
                let env_deid = callee_name
                    .as_deref()
                    .map(|n| self.env.is_deid_fn(n))
                    .unwrap_or(false);
                let fallback_deid = matches!(
                    callee_name.as_deref(),
                    Some("deidentify")
                        | Some("anonymize")
                        | Some("deidentify_patient")
                        | Some("deid")
                );
                if env_deid || fallback_deid {
                    acc = Anonymized;
                }

                // Fallback: pseudonymization routines produce Pseudonymized
                let is_pseudo = matches!(
                    callee_name.as_deref(),
                    Some("pseudonymize") | Some("pseudonymise") | Some("pseudo")
                );
                if is_pseudo {
                    acc = PrivacyAnnotation::Pseudonymized;
                }

                // Determine sink via env metadata first, fallback to known names
                let env_sink_kind: Option<SinkKind> =
                    callee_name.as_deref().and_then(|n| self.env.get_sink_fn(n));
                let fallback_is_sink = matches!(
                    callee_name.as_deref(),
                    Some("print")
                        | Some("println")
                        | Some("log")
                        | Some("debug")
                        | Some("trace")
                        | Some("export")
                        | Some("writeFile")
                        | Some("write_file")
                        | Some("save")
                        | Some("send_http")
                        | Some("http_post")
                        | Some("http_get")
                        | Some("network_send")
                        | Some("send")
                );

                if env_sink_kind.is_some() || fallback_is_sink {
                    // Determine sink kind and class
                    let kind = env_sink_kind;
                    // Policy: per-sink and per-kind minimum requirement
                    if let Some(k) = kind {
                        if let Some(min) = self
                            .env
                            .get_sink_min_privacy(callee_name.as_deref().unwrap_or(""))
                            .or_else(|| self.env.get_sink_kind_min_privacy(k))
                        {
                            if !crate::runtime::meets_min(acc, min) {
                                let name = callee_name.as_deref().unwrap_or("<unknown>");
                                self.errors.push(TypeError::PolicyViolation {
                                    reason: format!(
                                        "label {acc:?} does not meet minimum {min:?} for sink '{name}'"
                                    ),
                                });
                            }
                        }
                    }

                    // HIPAA core restrictions
                    let is_violation = match acc {
                        PHI => true,
                        PrivacyAnnotation::Pseudonymized => {
                            matches!(kind, Some(SinkKind::Network) | Some(SinkKind::File))
                        }
                        PrivacyAnnotation::AuthorizedFor(c) => match kind {
                            Some(SinkKind::Log) if c == medic_type::types::SinkClass::Log => false,
                            Some(SinkKind::Print) if c == medic_type::types::SinkClass::Print => {
                                false
                            }
                            Some(SinkKind::Export) if c == medic_type::types::SinkClass::Export => {
                                false
                            }
                            Some(SinkKind::Network)
                                if c == medic_type::types::SinkClass::Network =>
                            {
                                false
                            }
                            Some(SinkKind::File) if c == medic_type::types::SinkClass::File => {
                                false
                            }
                            Some(_) => true,
                            None => true,
                        },
                        _ => false,
                    };
                    if is_violation {
                        let name = callee_name.as_deref().unwrap_or("<unknown>");
                        let kind_str = env_sink_kind
                            .map(|k| match k {
                                SinkKind::Log => "log",
                                SinkKind::Print => "print",
                                SinkKind::Export => "export",
                                SinkKind::Network => "network",
                                SinkKind::File => "file",
                            })
                            .unwrap_or("sink");
                        self.errors.push(TypeError::PrivacyViolation {
                            reason: format!(
                                "HIPAA violation: {acc:?} passed to {kind_str} '{name}'",
                            ),
                        });
                    }
                }

                acc
            }
            ExpressionNode::HealthcareQuery(_) => PHI,
            ExpressionNode::Array(Spanned { node: arr, .. }) => {
                let mut acc = Anonymized;
                for el in &arr.elements {
                    acc = acc.join(self.check_expr_privacy(el));
                }
                acc
            }
            ExpressionNode::Struct(Spanned { node: s, .. }) => {
                // Heuristic: if any field appears PHI-like, mark PHI; otherwise anonymized
                let mut acc = Anonymized;
                for f in &s.fields {
                    acc = acc.join(self.check_expr_privacy(&f.value));
                }
                acc
            }
            ExpressionNode::Statement(Spanned { node: stmt, .. }) => match &**stmt {
                StatementNode::Expr(e) => self.check_expr_privacy(e),
                _ => Anonymized,
            },
        };
        let sp = expr.span();
        self.privacy_table.insert((sp.start, sp.end), label);
        label
    }

    /// Helper: best-effort extract identifier name from an expression (Identifier or Member callee).
    pub(crate) fn extract_identifier_name(expr: &ExpressionNode) -> Option<String> {
        match expr {
            ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                Some(ident.name().to_string())
            }
            ExpressionNode::Member(Spanned { node: mem, .. }) => {
                // Use property name as the callee if method call style
                Some(mem.property.name().to_string())
            }
            _ => None,
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
    ///     IdentifierNode::from_str_name("x"),
    ///     Span { start: 0, end: 1, line: 1, column: 1 },
    /// );
    /// let expr = ExpressionNode::Identifier(id);
    /// assert_eq!(checker.check_expr(&expr), MediType::Int);
    /// ```
    pub fn check_expr(&mut self, expr: &ExpressionNode) -> MediType {
        // Always compute privacy label alongside type
        let _privacy = self.check_expr_privacy(expr);
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
            // Quantity literal: use Quantity(Float) as a wrapper; unit tracked in side table
            ExpressionNode::Quantity(Spanned { .. }) => {
                MediType::Quantity(Box::new(MediType::Float))
            }
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
                        // quantity_ir: preliminary quantity rules (feature-gated)
                        #[cfg(feature = "quantity_ir")]
                        {
                            use BinaryOperator as Op;
                            // Enforce same-unit add/sub
                            if matches!(bin.operator, Op::Add | Op::Sub) {
                                let lu = Self::extract_quantity_unit(&bin.left);
                                let ru = Self::extract_quantity_unit(&bin.right);
                                match (lu, ru) {
                                    (Some(lu), Some(ru)) => {
                                        if lu != ru {
                                            self.errors.push(TypeError::ValidationFailed(
                                                ValidationError::InvalidUnit { unit: format!(
                                                    "quantity add/sub requires same units; found '{lu}' vs '{ru}'"
                                                )},
                                            ));
                                            return MediType::Unknown;
                                        }
                                    }
                                    (Some(_), None) | (None, Some(_)) => {
                                        self.errors.push(TypeError::ValidationFailed(
                                            ValidationError::InvalidUnit { unit: "quantity add/sub requires both operands to be quantities".into() },
                                        ));
                                        return MediType::Unknown;
                                    }
                                    _ => {}
                                }
                            }

                            // Allow Mul/Div over quantities by computing resulting dimension vector (for future use)
                            if matches!(bin.operator, Op::Mul | Op::Div) {
                                let ld = Self::extract_dimvec(&bin.left);
                                let rd = Self::extract_dimvec(&bin.right);
                                match (ld, rd) {
                                    (Some(ld), Some(rd)) => {
                                        let _res = if bin.operator == Op::Mul {
                                            units::combine_dims_mul(ld, rd)
                                        } else {
                                            units::combine_dims_div(ld, rd)
                                        };
                                        // Result is still a quantity value
                                        return MediType::Quantity(Box::new(MediType::Float));
                                    }
                                    // quantity with scalar: retain quantity dimension
                                    (Some(_), None) | (None, Some(_)) => {
                                        return MediType::Quantity(Box::new(MediType::Float));
                                    }
                                    _ => {}
                                }
                            }
                        }
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
                        // quantity_ir: allow comparisons when dimensions match
                        #[cfg(feature = "quantity_ir")]
                        {
                            let ld = Self::extract_dimvec(&bin.left);
                            let rd = Self::extract_dimvec(&bin.right);
                            match (ld, rd) {
                                (Some(ld), Some(rd)) => {
                                    if !units::dims_equal(ld, rd) {
                                        self.errors.push(TypeError::ValidationFailed(
                                            ValidationError::InvalidUnit { unit: "quantity comparisons require compatible dimensions".into() },
                                        ));
                                        return MediType::Unknown;
                                    }
                                }
                                // If only one side is quantity, reject
                                (Some(_), None) | (None, Some(_)) => {
                                    self.errors.push(TypeError::ValidationFailed(
                                        ValidationError::InvalidUnit {
                                            unit: "cannot compare quantity with non-quantity"
                                                .into(),
                                        },
                                    ));
                                    return MediType::Unknown;
                                }
                                _ => {}
                            }
                        }
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
                        // Extract units: left must be a quantity, right an identifier unit
                        let lhs_unit = Self::extract_quantity_unit(&bin.left);
                        let rhs_unit = match &bin.right {
                            ExpressionNode::Identifier(Spanned { node: id, .. }) => {
                                Some(id.name().to_string())
                            }
                            _ => None,
                        };

                        if let (Some(from_u), Some(to_u)) = (lhs_unit, rhs_unit) {
                            match units::factor_from_to(&from_u, &to_u) {
                                Ok(_factor) => {
                                    // Record resulting unit on this expression span
                                    // Result type: Quantity(Float)
                                    self.set_unit_at_span(expr.span(), &to_u);
                                    MediType::Quantity(Box::new(MediType::Float))
                                }
                                Err(msg) => {
                                    self.errors.push(TypeError::ValidationFailed(
                                        ValidationError::InvalidUnit { unit: msg },
                                    ));
                                    MediType::Unknown
                                }
                            }
                        } else {
                            // If units are not both provided as expected, fall back to right-hand type
                            right
                        }
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
            ExpressionNode::Call(Spanned {
                node: call,
                span: call_span,
            }) => {
                let callee_type = self.check_expr(&call.callee);
                let callee_name = Self::extract_identifier_name(&call.callee);
                if let MediType::Function {
                    params,
                    return_type,
                } = callee_type
                {
                    if params.len() == call.arguments.len() {
                        // Check argument types and enforce unit expectations when provided
                        for (idx, (arg, param_type)) in
                            call.arguments.iter().zip(params.iter()).enumerate()
                        {
                            let arg_type = self.check_expr(arg);
                            // Permit numeric scalar when a quantity is expected, so we can surface a missing-unit error
                            let expects_quantity = matches!(
                                param_type,
                                MediType::Quantity(inner) if **inner == MediType::Float
                            );
                            if expects_quantity {
                                let arg_ok = matches!(
                                    arg_type,
                                    MediType::Quantity(_) | MediType::Float | MediType::Int
                                );
                                if !arg_ok {
                                    return MediType::Unknown;
                                }
                            } else if arg_type != *param_type {
                                return MediType::Unknown;
                            }

                            // Enforce unit expectation, if any
                            if let Some(fname) = &callee_name {
                                if let Some(exp_unit) = self
                                    .param_unit_expectations
                                    .get(&(fname.clone(), idx))
                                    .cloned()
                                {
                                    if let Some(actual_unit) = Self::extract_quantity_unit(arg) {
                                        if let Err(msg) =
                                            units::factor_from_to(&actual_unit, &exp_unit)
                                        {
                                            self.errors.push(TypeError::ValidationFailed(
                                                ValidationError::InvalidUnit { unit: msg },
                                            ));
                                            return MediType::Unknown;
                                        } else {
                                            // Record normalized unit on the argument span
                                            self.set_unit_at_span(arg.span(), &exp_unit);
                                        }
                                    } else {
                                        // Missing unit on quantity-expected parameter
                                        self.errors.push(TypeError::ValidationFailed(
                                            ValidationError::InvalidUnit {
                                                unit: format!(
                                                    "missing unit for parameter {idx} of '{fname}' (expected '{exp_unit}')"
                                                ),
                                            },
                                        ));
                                        return MediType::Unknown;
                                    }
                                }
                            }
                        }

                        // Handle return unit expectation
                        if let Some(fname) = &callee_name {
                            if let Some(exp_ret) = self.return_unit_expectations.get(fname).cloned()
                            {
                                self.set_unit_at_span(call_span, &exp_ret);
                                return MediType::Quantity(Box::new(MediType::Float));
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
            ExpressionNode::Array(Spanned { node: arr, .. }) => {
                if arr.elements.is_empty() {
                    MediType::List(Box::new(MediType::Unknown))
                } else {
                    // Infer a unified element type; promote mixed numerics to Float, else Unknown
                    let mut iter = arr.elements.iter().map(|e| self.check_expr(e));
                    let mut elem_ty = iter.next().unwrap_or(MediType::Unknown);
                    for t in iter {
                        if t == elem_ty {
                            continue;
                        }
                        if t.is_numeric() && elem_ty.is_numeric() {
                            elem_ty = MediType::Float;
                        } else {
                            elem_ty = MediType::Unknown;
                            break;
                        }
                    }
                    MediType::List(Box::new(elem_ty))
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
            ExpressionNode::Index(Spanned { node: idx, .. }) => {
                // If object is a list, type is its element type; otherwise Unknown
                let obj_ty = self.check_expr(&idx.object);
                let _ = self.check_expr(&idx.index);
                match obj_ty {
                    MediType::List(inner) => *inner,
                    _ => MediType::Unknown,
                }
            }
            // (call arm handled earlier in this match)
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

                // If annotation corresponds to a privacy label, set it for the binding
                if let Some(ann) = let_stmt
                    .type_annotation
                    .as_ref()
                    .and_then(Self::resolve_privacy_annotation)
                {
                    self.env.set_privacy(let_stmt.name.name().to_string(), ann);
                }

                // Check optional initializer
                let init_ty = let_stmt.value.as_ref().map(|expr| self.check_expr(expr));
                // Privacy of initializer
                let init_priv = let_stmt
                    .value
                    .as_ref()
                    .map(|expr| self.check_expr_privacy(expr));

                // If binding already has a privacy label, enforce flow from initializer
                if let Some(dst_label) = self.env.get_privacy(let_stmt.name.name()) {
                    if let Some(src_label) = init_priv {
                        if !Self::privacy_flow_allowed(src_label, dst_label) {
                            self.errors.push(TypeError::PrivacyViolation {
                                reason: format!(
                                    "Illegal data flow: {:?} -> {:?} for binding '{}'",
                                    src_label,
                                    dst_label,
                                    let_stmt.name.name()
                                ),
                            });
                        }
                    }
                } else if let Some(src_label) = init_priv {
                    // Default: set privacy to source label on first definition
                    self.env
                        .set_privacy(let_stmt.name.name().to_string(), src_label);
                }

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
                let value_priv = self.check_expr_privacy(&assign.value);
                match &assign.target {
                    ExpressionNode::Identifier(Spanned { node: ident, .. }) => {
                        if let Some(target_ty) = self.env.get(ident.name()) {
                            if value_ty.is_assignable_to(target_ty) {
                                // Privacy enforcement
                                if let Some(dst_priv) = self.env.get_privacy(ident.name()) {
                                    if !Self::privacy_flow_allowed(value_priv, dst_priv) {
                                        self.errors.push(TypeError::PrivacyViolation {
                                            reason: format!(
                                                "Illegal data flow: {:?} -> {:?} for '{}')",
                                                value_priv,
                                                dst_priv,
                                                ident.name()
                                            ),
                                        });
                                    }
                                } else {
                                    // If target had no privacy, inherit from source
                                    self.env.set_privacy(ident.name().to_string(), value_priv);
                                }
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
            StatementNode::Return(ret) => {
                if let Some(expr) = &ret.value {
                    let privy = self.check_expr_privacy(expr);
                    if matches!(privy, PrivacyAnnotation::PHI) {
                        self.errors.push(TypeError::PrivacyViolation {
                            reason: "HIPAA violation: returning PHI across function boundary"
                                .to_string(),
                        });
                    }
                    let _ = self.check_expr(expr);
                }
                Ok(())
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
            StatementNode::Regulate(reg) => {
                // Check body in new scope
                let parent = self.env.clone();
                let mut child = TypeEnv::with_parent(parent);
                let mut ck = TypeChecker::new(&mut child);
                if let Some(ctx) = self.validation_ctx {
                    ck = ck.with_validation_ctx(ctx);
                }
                for s in &reg.body.statements {
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
    /// Privacy/access-control violations (e.g., PHI -> Anonymized flow)
    PrivacyViolation {
        reason: String,
    },
    /// Policy violations (environment-configured minimums, etc.)
    PolicyViolation {
        reason: String,
    },
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
            TypeError::PrivacyViolation { reason } => write!(f, "Privacy violation: {reason}"),
            TypeError::PolicyViolation { reason } => write!(f, "Policy violation: {reason}"),
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
                statements: NodeList::new(),
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
            params: {
                let mut p: NodeList<ParameterNode> = NodeList::new();
                p.extend([
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
                ]);
                p
            },
            return_type: Some(ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("Int"),
                Span::default(),
            ))),
            body: BlockNode {
                statements: {
                    let mut s: NodeList<StatementNode> = NodeList::new();
                    s.extend([StatementNode::Let(Box::new(LetStatementNode {
                        name: IdentifierNode::from_str_name("x"),
                        type_annotation: None,
                        value: Some(ExpressionNode::Literal(Spanned::new(
                            LiteralNode::Int(1),
                            Span::default(),
                        ))),
                        span: Span::default(),
                    }))]);
                    s
                },
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
                arguments: {
                    let mut a: NodeList<ExpressionNode> = NodeList::new();
                    a.extend([
                        ExpressionNode::Literal(Spanned::new(LiteralNode::Int(1), arg1_span)),
                        ExpressionNode::Literal(Spanned::new(LiteralNode::Int(2), arg2_span)),
                    ]);
                    a
                },
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
