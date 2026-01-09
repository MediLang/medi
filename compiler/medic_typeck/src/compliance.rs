use medic_ast::ast::{BlockNode, ExpressionNode, ProgramNode, Spanned, StatementNode};
use medic_ast::visit::Span;
use medic_type::types::PrivacyAnnotation;

use crate::type_checker::TypeChecker;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComplianceRule {
    /// PHI or pseudonymized data flowing into a sink without anonymization/authorization.
    UnprotectedPhiToSink,
    /// PHI flowing out of a function boundary via return.
    PhiReturned,
    /// Environment policy violation (minimum privacy requirement not met).
    PolicyViolation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ComplianceViolation {
    pub rule: ComplianceRule,
    pub message: String,
    pub span: Option<Span>,
}

/// Lightweight compliance checker that leverages the type checker's privacy
/// inference and policy enforcement to surface compiler-level compliance findings.
///
/// This does not duplicate full data-flow analysis; it turns the privacy
/// metadata and existing type/policy errors into user-facing compliance
/// violations that can be surfaced as a dedicated stage.
pub fn check_compliance<'env>(
    checker: &TypeChecker<'env>,
    program: &ProgramNode,
) -> Vec<ComplianceViolation> {
    let mut violations = Vec::new();
    // Map any accumulated privacy/policy errors into compliance violations.
    for err in checker.errors() {
        match err {
            crate::type_checker::TypeError::PrivacyViolation { reason } => {
                violations.push(ComplianceViolation {
                    rule: ComplianceRule::UnprotectedPhiToSink,
                    message: reason.clone(),
                    span: None,
                })
            }
            crate::type_checker::TypeError::PolicyViolation { reason } => {
                violations.push(ComplianceViolation {
                    rule: ComplianceRule::PolicyViolation,
                    message: reason.clone(),
                    span: None,
                })
            }
            _ => {}
        }
    }

    // Walk the AST and use the privacy side table to catch unprotected PHI sink
    // usage and PHI returns even if callers ignore the type errors.
    for stmt in &program.statements {
        visit_stmt(stmt, checker, &mut violations);
    }

    violations
}

fn visit_stmt<'env>(
    stmt: &StatementNode,
    checker: &TypeChecker<'env>,
    violations: &mut Vec<ComplianceViolation>,
) {
    match stmt {
        StatementNode::Return(ret) => {
            if let Some(expr) = &ret.value {
                let span = expr.span();
                if let Some(label) = checker.privacy_label_for_span(span) {
                    if matches!(label, PrivacyAnnotation::PHI) {
                        violations.push(ComplianceViolation {
                            rule: ComplianceRule::PhiReturned,
                            message: "PHI returned from function boundary".to_string(),
                            span: Some(*span),
                        });
                    }
                }
                visit_expr(expr, checker, violations);
            }
        }
        StatementNode::Block(block) => visit_block(block, checker, violations),
        StatementNode::If(if_node) => {
            visit_block(&if_node.then_branch, checker, violations);
            if let Some(else_stmt) = &if_node.else_branch {
                visit_stmt(else_stmt, checker, violations);
            }
        }
        StatementNode::While(while_node) => {
            visit_expr(&while_node.condition, checker, violations);
            visit_block(&while_node.body, checker, violations);
        }
        StatementNode::For(for_node) => {
            visit_expr(&for_node.iterable, checker, violations);
            visit_block(&for_node.body, checker, violations);
        }
        StatementNode::Expr(expr) => visit_expr(expr, checker, violations),
        StatementNode::Function(fun) => {
            visit_block(&fun.body, checker, violations);
        }
        // TypeDecl, Assignment, Let, Match handled by scanning their inner expressions
        StatementNode::Let(let_stmt) => {
            if let Some(v) = &let_stmt.value {
                visit_expr(v, checker, violations);
            }
        }
        StatementNode::Assignment(assign) => visit_expr(&assign.value, checker, violations),
        StatementNode::Regulate(reg) => visit_block(&reg.body, checker, violations),
        StatementNode::Federated(fed) => visit_block(&fed.body, checker, violations),
        StatementNode::TypeDecl(_) | StatementNode::Match(_) => {}
    }
}

fn visit_block<'env>(
    block: &BlockNode,
    checker: &TypeChecker<'env>,
    violations: &mut Vec<ComplianceViolation>,
) {
    for s in &block.statements {
        visit_stmt(s, checker, violations);
    }
}

fn visit_expr<'env>(
    expr: &ExpressionNode,
    checker: &TypeChecker<'env>,
    violations: &mut Vec<ComplianceViolation>,
) {
    match expr {
        ExpressionNode::Call(Spanned { node: call, span }) => {
            // Determine privacy label for this call expression.
            if let Some(label) = checker.privacy_label_for_span(span) {
                if matches!(
                    label,
                    PrivacyAnnotation::PHI | PrivacyAnnotation::Pseudonymized
                ) {
                    if let Some(callee) = TypeChecker::extract_identifier_name(&call.callee) {
                        // If the call is to a known sink, report unprotected PHI.
                        if checker.env().get_sink_fn(&callee).is_some() {
                            violations.push(ComplianceViolation {
                                rule: ComplianceRule::UnprotectedPhiToSink,
                                message: format!(
                                    "PHI-like data flows into sink '{callee}'; de-identify or authorize",
                                ),
                                span: Some(*span),
                            });
                        }
                    }
                }
            }
            // Recurse into callee/arguments.
            visit_expr(&call.callee, checker, violations);
            for a in &call.arguments {
                visit_expr(a, checker, violations);
            }
        }
        ExpressionNode::Binary(Spanned { node: bin, .. }) => {
            visit_expr(&bin.left, checker, violations);
            visit_expr(&bin.right, checker, violations);
        }
        ExpressionNode::Member(Spanned { node: mem, .. }) => {
            visit_expr(&mem.object, checker, violations);
        }
        ExpressionNode::Index(Spanned { node: idx, .. }) => {
            visit_expr(&idx.object, checker, violations);
            visit_expr(&idx.index, checker, violations);
        }
        ExpressionNode::Array(Spanned { node: arr, .. }) => {
            for el in &arr.elements {
                visit_expr(el, checker, violations);
            }
        }
        ExpressionNode::Struct(Spanned { node: s, .. }) => {
            for f in &s.fields {
                visit_expr(&f.value, checker, violations);
            }
        }
        ExpressionNode::Statement(Spanned { node: stmt, .. }) => {
            visit_stmt(stmt, checker, violations)
        }
        // Terminals
        ExpressionNode::Identifier(_)
        | ExpressionNode::Literal(_)
        | ExpressionNode::Quantity(_)
        | ExpressionNode::IcdCode(_)
        | ExpressionNode::CptCode(_)
        | ExpressionNode::SnomedCode(_)
        | ExpressionNode::HealthcareQuery(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use medic_ast::ast::*;
    use medic_env::env::TypeEnv;

    #[test]
    fn detects_phi_returned_from_function_boundary() {
        let mut env = TypeEnv::with_prelude();
        // Seed privacy metadata for phi_val
        env.set_privacy("phi_val".to_string(), PrivacyAnnotation::PHI);
        let mut tc = TypeChecker::new(&mut env);

        // Build program: return phi_val;
        let ret_stmt = StatementNode::Return(Box::new(ReturnNode {
            value: Some(ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name("phi_val"),
                Span::default(),
            ))),
            span: Span::default(),
        }));
        let mut stmts: NodeList<StatementNode> = NodeList::new();
        stmts.extend([ret_stmt]);
        let program = ProgramNode { statements: stmts };

        // Run type checker to populate privacy table.
        let _ = tc.check_program(&program);

        let violations = check_compliance(&tc, &program);
        assert!(
            violations
                .iter()
                .any(|v| matches!(v.rule, ComplianceRule::PhiReturned)),
            "Expected a PHI return violation"
        );
    }

    #[test]
    fn detects_unprotected_phi_flow_to_sink() {
        let mut env = TypeEnv::with_prelude();
        env.set_privacy("patient_data".to_string(), PrivacyAnnotation::PHI);
        env.set_sink_fn("external_log", medic_env::env::SinkKind::Log);
        let mut tc = TypeChecker::new(&mut env);

        // external_log(patient_data)
        let call_stmt = StatementNode::Expr(ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ExpressionNode::Identifier(Spanned::new(
                    IdentifierNode::from_str_name("external_log"),
                    Span::default(),
                )),
                arguments: {
                    let mut args = NodeList::new();
                    args.extend([ExpressionNode::Identifier(Spanned::new(
                        IdentifierNode::from_str_name("patient_data"),
                        Span::default(),
                    ))]);
                    args
                },
            }),
            Span::default(),
        )));

        let mut stmts = NodeList::new();
        stmts.extend([call_stmt]);
        let program = ProgramNode { statements: stmts };

        // Run type check first to populate tables
        let _ = tc.check_program(&program);

        let violations = check_compliance(&tc, &program);
        assert!(
            violations
                .iter()
                .any(|v| matches!(v.rule, ComplianceRule::UnprotectedPhiToSink)),
            "Expected unprotected PHI sink violation"
        );
    }

    #[test]
    fn allows_anonymized_flow_to_sink() {
        let mut env = TypeEnv::with_prelude();
        env.set_privacy("anon_data".to_string(), PrivacyAnnotation::Anonymized);
        env.set_sink_fn("external_log", medic_env::env::SinkKind::Log);
        let mut tc = TypeChecker::new(&mut env);

        // external_log(anon_data)
        let call_stmt = StatementNode::Expr(ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ExpressionNode::Identifier(Spanned::new(
                    IdentifierNode::from_str_name("external_log"),
                    Span::default(),
                )),
                arguments: {
                    let mut args = NodeList::new();
                    args.extend([ExpressionNode::Identifier(Spanned::new(
                        IdentifierNode::from_str_name("anon_data"),
                        Span::default(),
                    ))]);
                    args
                },
            }),
            Span::default(),
        )));

        let mut stmts = NodeList::new();
        stmts.extend([call_stmt]);
        let program = ProgramNode { statements: stmts };

        let _ = tc.check_program(&program);
        let violations = check_compliance(&tc, &program);
        assert!(
            violations.is_empty(),
            "Expected no violations for anonymized data"
        );
    }
}
