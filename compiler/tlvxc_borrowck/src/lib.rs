use tlvxc_ast::ast::*;
use tlvxc_ast::visit::{Span, VisitResult, Visitable, Visitor};
use tlvxc_type::types::PrivacyAnnotation;
use std::collections::HashMap;

#[derive(Debug, thiserror::Error)]
pub enum BorrowError {
    #[error("{0}")]
    Message(String),
}

// --- Real-time constraint checker (opt-in markers) ---

/// Checks that within regions delimited by calls to `rt_begin` and `rt_end`,
/// no calls to disallowed functions occur (e.g., GC, dynamic allocation, spawning, channels).
/// The checker is conservative and string-based over callee names.
pub struct RtConstraintChecker {
    begin_fn: String,
    end_fn: String,
    disallowed: std::collections::HashSet<String>,
    errors: Vec<String>,
    inside_rt: bool,
}

impl RtConstraintChecker {
    pub fn new(begin_fn: &str, end_fn: &str, disallowed: impl IntoIterator<Item = String>) -> Self {
        Self {
            begin_fn: begin_fn.to_string(),
            end_fn: end_fn.to_string(),
            disallowed: disallowed.into_iter().collect(),
            errors: Vec::new(),
            inside_rt: false,
        }
    }

    pub fn check_program(mut self, program: &ProgramNode) -> Result<(), Vec<String>> {
        let _ = program.accept(&mut self);
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors)
        }
    }
}

impl Visitor for RtConstraintChecker {
    type Output = ();

    fn visit_program(&mut self, node: &ProgramNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_block(&mut self, node: &BlockNode) -> VisitResult<Self::Output> {
        self.visit_children(node)
    }

    fn visit_call_expr(&mut self, node: &CallExpressionNode) -> VisitResult<Self::Output> {
        // Gather potential callee names: full member path and last segment
        fn callee_names(expr: &ExpressionNode) -> (Option<String>, Option<String>, Span) {
            match expr {
                ExpressionNode::Identifier(Spanned {
                    node: IdentifierNode { name },
                    span,
                }) => (Some(name.to_string()), Some(name.to_string()), *span),
                ExpressionNode::Member(Spanned { node: boxed, span }) => {
                    // Unwind chain to build a.b.c and last segment c
                    let mut parts: Vec<String> = vec![boxed.property.name().to_string()];
                    let mut cur: &ExpressionNode = &boxed.object;
                    let base_span = *span;
                    loop {
                        match cur {
                            ExpressionNode::Member(Spanned { node: inner, .. }) => {
                                parts.push(inner.property.name().to_string());
                                cur = &inner.object;
                            }
                            ExpressionNode::Identifier(Spanned {
                                node: IdentifierNode { name },
                                span: s,
                            }) => {
                                parts.push(name.to_string());
                                parts.reverse();
                                let full = parts.join(".");
                                let last = parts.last().cloned();
                                return (Some(full), last, *s);
                            }
                            _ => return (None, None, base_span),
                        }
                    }
                }
                other => {
                    let sp = other.span();
                    (None, None, *sp)
                }
            }
        }

        let (full, last, span) = callee_names(&node.callee);
        if let Some(name) = full.as_ref().or(last.as_ref()) {
            if name == &self.begin_fn {
                self.inside_rt = true;
            } else if name == &self.end_fn {
                self.inside_rt = false;
            } else if self.inside_rt {
                let banned = match (full.as_ref(), last.as_ref()) {
                    (Some(fq), Some(ls)) => {
                        self.disallowed.contains(fq) || self.disallowed.contains(ls)
                    }
                    (Some(fq), None) => self.disallowed.contains(fq),
                    (None, Some(ls)) => self.disallowed.contains(ls),
                    _ => false,
                };
                if banned {
                    self.errors.push(format!(
                        "disallowed call '{}' inside RT section at line {} col {}",
                        name, span.line, span.column
                    ));
                }
            }
        }
        // Recurse into callee and args (although callee is simple in current language)
        node.callee.accept(self)?;
        for arg in &node.arguments {
            arg.accept(self)?;
        }
        Ok(())
    }
}

#[cfg(test)]
#[allow(clippy::useless_conversion)]
mod rt_tests {
    use super::*;

    fn call(name: &str) -> ExpressionNode {
        ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ExpressionNode::Identifier(Spanned::new(
                    IdentifierNode::from_str_name(name),
                    Span::default(),
                )),
                arguments: vec![].into(),
            }),
            Span::default(),
        ))
    }

    #[test]
    fn rt_checker_flags_disallowed_calls_between_markers() {
        let program = ProgramNode {
            statements: vec![
                StatementNode::Expr(call("rt_begin")),
                StatementNode::Expr(call("tolvex_gc_alloc_string")),
                StatementNode::Expr(call("rt_end")),
            ]
            .into(),
        };
        let disallowed = [
            "tolvex_gc_alloc_string".to_string(),
            "spawn_task".to_string(),
            "create_channel".to_string(),
        ];
        let checker = RtConstraintChecker::new("rt_begin", "rt_end", disallowed);
        let res = checker.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn rt_checker_allows_calls_outside_markers() {
        let program = ProgramNode {
            statements: vec![StatementNode::Expr(call("tolvex_gc_alloc_string"))].into(),
        };
        let disallowed = ["tolvex_gc_alloc_string".to_string()];
        let checker = RtConstraintChecker::new("rt_begin", "rt_end", disallowed);
        let res = checker.check_program(&program);
        assert!(res.is_ok());
    }
}

/// Opaque variable identifier allocated on declaration.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(u32);

/// Partial borrow path used for member field tracking
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct FieldPath(Vec<String>);

#[derive(Debug, Clone, Default)]
struct AliasSummary {
    /// If Some(i), function returns a reference to the i-th argument
    returns_ref_to_arg: Option<usize>,
}

#[derive(Default)]
struct SymbolTable {
    scopes: Vec<HashMap<String, VarId>>, // name -> VarId
    next_id: u32,
}

impl SymbolTable {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
            next_id: 0,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &str) -> VarId {
        let id = VarId(self.next_id);
        self.next_id += 1;
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), id);
        }
        id
    }

    // (tests are defined below inside #[cfg(test)] mod tests)

    fn resolve(&self, name: &str) -> Option<VarId> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.get(name) {
                return Some(*id);
            }
        }
        None
    }
}

#[cfg(test)]
#[allow(clippy::useless_conversion)]
mod tests {
    use super::*;
    use tlvxc_ast::visit::Span;

    fn ident(name: &str) -> ExpressionNode {
        ExpressionNode::Identifier(Spanned::new(
            IdentifierNode::from_str_name(name),
            Span::default(),
        ))
    }

    fn let_stmt(name: &str, value: Option<ExpressionNode>) -> StatementNode {
        StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name(name),
            type_annotation: None,
            value,
            span: Span::default(),
        }))
    }

    fn assign_stmt(name: &str, value: ExpressionNode) -> StatementNode {
        StatementNode::Assignment(Box::new(AssignmentNode {
            target: ident(name),
            value,
            span: Span::default(),
        }))
    }

    fn member_expr(base: ExpressionNode, prop: &str) -> ExpressionNode {
        ExpressionNode::Member(Spanned::new(
            Box::new(MemberExpressionNode {
                object: base,
                property: IdentifierNode::from_str_name(prop),
            }),
            Span::default(),
        ))
    }

    fn call_expr(callee: ExpressionNode, args: Vec<ExpressionNode>) -> ExpressionNode {
        ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee,
                arguments: args.into(),
            }),
            Span::default(),
        ))
    }

    #[test]
    fn borrow_checker_allows_simple_reads_and_assign() {
        // let x = 1; x = 2; (reads and a simple assignment)
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "x",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(1),
                        Span::default(),
                    ))),
                ),
                assign_stmt(
                    "x",
                    ExpressionNode::Literal(Spanned::new(LiteralNode::Int(2), Span::default())),
                ),
                StatementNode::Expr(ident("x")),
            ]
            .into(),
        };

        let mut b = BorrowChecker::new();
        assert!(b.check_program(&program).is_ok());
    }

    #[test]
    fn borrow_checker_reports_conflicting_borrows() {
        // Pattern that trips current checker: x is mutably borrowed as assignment target,
        // then read immutably in the value expression (x = x;)
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "x",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                assign_stmt("x", ident("x")),
            ]
            .into(),
        };

        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    // New tests exercising indexing and alias summaries

    fn index_expr(base: ExpressionNode, idx: i64) -> ExpressionNode {
        let idx_expr =
            ExpressionNode::Literal(Spanned::new(LiteralNode::Int(idx), Span::default()));
        let span = Span::default();
        ExpressionNode::Index(Spanned::new(
            Box::new(IndexExpressionNode {
                object: base,
                index: idx_expr,
            }),
            span,
        ))
    }

    #[test]
    fn spawn_task_argument_conflicts_with_mut_borrow_in_assignment() {
        // let x = 0; x = spawn_task(x);
        // LHS mutably borrows x, then spawn_task tries to capture/share x.
        let call = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ident("spawn_task"),
                arguments: vec![ident("x")].into(),
            }),
            Span::default(),
        ));

        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "x",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: ident("x"),
                    value: call,
                    span: Span::default(),
                })),
            ]
            .into(),
        };

        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn indexing_rhs_conflicts_with_whole_lhs_assignment() {
        // let arr = 0; arr = arr[0]; -> mutable borrow of arr (LHS) conflicts with immutable borrow in RHS index
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "arr",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: ident("arr"),
                    value: index_expr(ident("arr"), 0),
                    span: Span::default(),
                })),
            ]
            .into(),
        };
        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn indexing_rhs_conflicts_with_indexed_lhs_assignment() {
        // let arr = 0; arr[1] = arr[0]; -> partial mutable borrow on arr[*] conflicts with read of arr in RHS
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "arr",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: index_expr(ident("arr"), 1),
                    value: index_expr(ident("arr"), 0),
                    span: Span::default(),
                })),
            ]
            .into(),
        };
        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn alias_summary_conflict_on_assignment() {
        // Model: let x = 0; x = ret_ref_to_arg(x);
        // LHS mutable borrow of x conflicts with immutable borrow of x inside call due to alias summary
        let call = ExpressionNode::Call(Spanned::new(
            Box::new(CallExpressionNode {
                callee: ident("ret_ref_to_arg"),
                arguments: vec![ident("x")].into(),
            }),
            Span::default(),
        ));

        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "x",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: ident("x"),
                    value: call,
                    span: Span::default(),
                })),
            ]
            .into(),
        };

        let mut b = BorrowChecker::new();
        b.register_alias_summary("ret_ref_to_arg", Some(0));
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn member_property_is_not_treated_as_variable() {
        // Ensure `patient.ssn` does not create an artificial read of variable `ssn`.
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "patient",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Expr(member_expr(ident("patient"), "ssn")),
            ]
            .into(),
        };
        let mut b = BorrowChecker::new();
        assert!(b.check_program(&program).is_ok());
    }

    #[test]
    fn member_rhs_conflicts_with_member_lhs_assignment() {
        // let patient = 0; patient.ssn = patient.ssn;
        // Field-level mutable borrow on LHS conflicts with immutable borrow of same field on RHS.
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "patient",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: member_expr(ident("patient"), "ssn"),
                    value: member_expr(ident("patient"), "ssn"),
                    span: Span::default(),
                })),
            ]
            .into(),
        };
        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn channel_send_argument_conflicts_with_mut_borrow_in_assignment() {
        // let x = 0; let tx = 0; x = tx.send(x);
        // LHS mutably borrows x, then send() tries to share x.
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "x",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                let_stmt(
                    "tx",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Assignment(Box::new(AssignmentNode {
                    target: ident("x"),
                    value: call_expr(member_expr(ident("tx"), "send"), vec![ident("x")]),
                    span: Span::default(),
                })),
            ]
            .into(),
        };
        let mut b = BorrowChecker::new();
        let res = b.check_program(&program);
        assert!(res.is_err());
    }

    #[test]
    fn phi_value_cannot_be_shared_across_spawn_task() {
        // Privacy-aware borrowing: PHI variables cannot cross task boundaries.
        let program = ProgramNode {
            statements: vec![
                let_stmt(
                    "patient",
                    Some(ExpressionNode::Literal(Spanned::new(
                        LiteralNode::Int(0),
                        Span::default(),
                    ))),
                ),
                StatementNode::Expr(ExpressionNode::Call(Spanned::new(
                    Box::new(CallExpressionNode {
                        callee: ident("spawn_task"),
                        arguments: vec![ident("patient")].into(),
                    }),
                    Span::default(),
                ))),
            ]
            .into(),
        };

        let mut b =
            BorrowChecker::with_privacy_labels([("patient".to_string(), PrivacyAnnotation::PHI)]);
        let res = b.check_program(&program);
        assert!(res.is_err());
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum BorrowState {
    #[default]
    Free,
    ImmutableBorrow,
    MutableBorrow,
}

#[derive(Default)]
pub struct BorrowChecker {
    // Symbol table for declarations
    sym: SymbolTable,
    // Current borrow state per VarId
    states: HashMap<VarId, BorrowState>,
    // Partial borrow tracking: (VarId, FieldPath) -> BorrowState
    field_states: HashMap<(VarId, FieldPath), BorrowState>,
    // Region stack: track which vars were borrowed in the region for release at exit
    region_stack: Vec<Vec<VarId>>,
    // Diagnostics
    errors: Vec<String>,
    // Function alias summaries: callee name -> summary
    alias_summaries: HashMap<String, AliasSummary>,

    // Optional privacy labels per variable name (from type checking).
    // When present, we enforce additional privacy-aware sharing rules.
    privacy_labels: HashMap<String, PrivacyAnnotation>,
}

impl BorrowChecker {
    pub fn new() -> Self {
        Self {
            sym: SymbolTable::new(),
            states: HashMap::new(),
            field_states: HashMap::new(),
            region_stack: Vec::new(),
            errors: Vec::new(),
            alias_summaries: HashMap::new(),
            privacy_labels: HashMap::new(),
        }
    }

    pub fn with_privacy_labels(
        labels: impl IntoIterator<Item = (String, PrivacyAnnotation)>,
    ) -> Self {
        let mut s = Self::new();
        s.privacy_labels = labels.into_iter().collect();
        s
    }

    /// Register a simple function alias summary. For now we only support
    /// modeling that the function returns a reference to one of its arguments.
    pub fn register_alias_summary(&mut self, fn_name: &str, returns_ref_to_arg: Option<usize>) {
        self.alias_summaries
            .insert(fn_name.to_string(), AliasSummary { returns_ref_to_arg });
    }

    pub fn check_program(&mut self, program: &ProgramNode) -> Result<(), Vec<String>> {
        self.sym.enter_scope();
        self.enter_region();
        let result = program.accept(self);
        self.exit_region();
        self.sym.exit_scope();
        if let Err(e) = result {
            self.errors.push(format!("visitor error: {e}"));
        }
        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn enter_scope(&mut self) {
        self.sym.enter_scope();
    }
    fn exit_scope(&mut self) {
        self.sym.exit_scope();
    }

    fn enter_region(&mut self) {
        self.region_stack.push(Vec::new());
    }
    fn exit_region(&mut self) {
        if let Some(vars) = self.region_stack.pop() {
            for v in vars {
                // Release to Free at region exit (scaffolding; future: restore previous state)
                self.states.insert(v, BorrowState::Free);
                // Release all field sub-borrows as well
                self.field_states.retain(|(var, _), _| *var != v);
            }
        }
    }

    fn declare(&mut self, name: &str) -> VarId {
        let id = self.sym.declare(name);
        self.states.insert(id, BorrowState::Free);
        id
    }

    fn mark_borrowed_in_region(&mut self, id: VarId) {
        if let Some(top) = self.region_stack.last_mut() {
            if !top.contains(&id) {
                top.push(id);
            }
        }
    }

    fn set_state(&mut self, id: VarId, state: BorrowState) {
        self.states.insert(id, state);
        if !matches!(state, BorrowState::Free) {
            self.mark_borrowed_in_region(id);
        }
    }

    fn get_state(&self, id: VarId) -> BorrowState {
        *self.states.get(&id).unwrap_or(&BorrowState::Free)
    }

    // --- CFG merge helpers (prototype) ---
    fn merge_state(a: BorrowState, b: BorrowState) -> BorrowState {
        if a == b {
            a
        } else {
            BorrowState::Free
        }
    }

    fn merge_maps<K: std::cmp::Eq + std::hash::Hash + Clone>(
        &self,
        a: &HashMap<K, BorrowState>,
        b: &HashMap<K, BorrowState>,
    ) -> HashMap<K, BorrowState> {
        let mut out: HashMap<K, BorrowState> = HashMap::new();
        // union of keys
        for (k, va) in a.iter() {
            let vb = b.get(k).copied().unwrap_or(BorrowState::Free);
            let m = Self::merge_state(*va, vb);
            if m != BorrowState::Free {
                out.insert(k.clone(), m);
            }
        }
        for (k, vb) in b.iter() {
            if out.contains_key(k) {
                continue;
            }
            let va = a.get(k).copied().unwrap_or(BorrowState::Free);
            let m = Self::merge_state(va, *vb);
            if m != BorrowState::Free {
                out.insert(k.clone(), m);
            }
        }
        out
    }

    // --- Partial borrow helpers ---

    fn resolve_lvalue(
        &mut self,
        expr: &ExpressionNode,
    ) -> Option<(VarId, Option<FieldPath>, Span)> {
        match expr {
            ExpressionNode::Identifier(Spanned {
                node: IdentifierNode { name },
                span,
            }) => {
                let id = self.resolve_or_declare(name.as_str());
                Some((id, None, *span))
            }
            ExpressionNode::Member(Spanned { node: boxed, .. }) => {
                // Unwind chain of Member/Index until base Identifier
                let mut props: Vec<String> = Vec::new();
                let mut cur_expr: &ExpressionNode = &boxed.object;
                props.push(boxed.property.name().to_string());
                loop {
                    match cur_expr {
                        ExpressionNode::Member(Spanned { node: inner, .. }) => {
                            props.push(inner.property.name().to_string());
                            cur_expr = &inner.object;
                        }
                        ExpressionNode::Index(Spanned { node: inner, .. }) => {
                            // Treat index as wildcard segment
                            props.push("*".to_string());
                            cur_expr = &inner.object;
                        }
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            span,
                        }) => {
                            let id = self.resolve_or_declare(name.as_str());
                            props.reverse();
                            return Some((id, Some(FieldPath(props)), *span));
                        }
                        _ => return None,
                    }
                }
            }
            ExpressionNode::Index(Spanned { node: boxed, .. }) => {
                // Unwind chain of Index/Member until base Identifier
                let mut props: Vec<String> = vec!["*".to_string()];
                let mut cur_expr: &ExpressionNode = &boxed.object;
                loop {
                    match cur_expr {
                        ExpressionNode::Member(Spanned { node: inner, .. }) => {
                            props.push(inner.property.name().to_string());
                            cur_expr = &inner.object;
                        }
                        ExpressionNode::Index(Spanned { node: inner, .. }) => {
                            props.push("*".to_string());
                            cur_expr = &inner.object;
                        }
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            span,
                        }) => {
                            let id = self.resolve_or_declare(name.as_str());
                            props.reverse();
                            return Some((id, Some(FieldPath(props)), *span));
                        }
                        _ => return None,
                    }
                }
            }
            _ => None,
        }
    }

    fn path_overlaps(a: &FieldPath, b: &FieldPath) -> bool {
        let min = a.0.len().min(b.0.len());
        a.0.iter().take(min).zip(b.0.iter()).all(|(x, y)| x == y)
    }

    fn get_field_state(&self, id: VarId, path: &FieldPath) -> BorrowState {
        // If whole var borrowed mutably/immutably, that dominates
        match self.get_state(id) {
            BorrowState::MutableBorrow => return BorrowState::MutableBorrow,
            BorrowState::ImmutableBorrow => return BorrowState::ImmutableBorrow,
            BorrowState::Free => {}
        }
        // Otherwise check overlapping field paths
        for ((vid, fp), st) in self.field_states.iter() {
            if *vid == id && Self::path_overlaps(fp, path) {
                if matches!(st, BorrowState::MutableBorrow) {
                    return BorrowState::MutableBorrow;
                }
                if matches!(st, BorrowState::ImmutableBorrow) {
                    return BorrowState::ImmutableBorrow;
                }
            }
        }
        BorrowState::Free
    }

    fn set_field_state(&mut self, id: VarId, path: FieldPath, state: BorrowState) {
        self.field_states.insert((id, path), state);
        self.mark_borrowed_in_region(id);
    }

    fn has_any_field_mut_borrow(&self, id: VarId) -> bool {
        self.field_states
            .iter()
            .any(|((vid, _), st)| *vid == id && matches!(st, BorrowState::MutableBorrow))
    }

    fn borrow_immutable_field(&mut self, id: VarId, path: FieldPath, span: Span) {
        match self.get_field_state(id, &path) {
            BorrowState::MutableBorrow => self.errors.push(format!(
                "cannot immutably borrow field while it is mutably borrowed at line {} col {}",
                span.line, span.column
            )),
            _ => self.set_field_state(id, path, BorrowState::ImmutableBorrow),
        }
    }

    fn resolve_access_path(&mut self, expr: &ExpressionNode) -> Option<(VarId, FieldPath, Span)> {
        match expr {
            ExpressionNode::Member(Spanned { node: boxed, span }) => {
                let mut parts: Vec<String> = vec![boxed.property.name().to_string()];
                let mut cur: &ExpressionNode = &boxed.object;
                loop {
                    match cur {
                        ExpressionNode::Member(Spanned { node: inner, .. }) => {
                            parts.push(inner.property.name().to_string());
                            cur = &inner.object;
                        }
                        ExpressionNode::Index(Spanned { node: inner, .. }) => {
                            parts.push("*".to_string());
                            cur = &inner.object;
                        }
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            ..
                        }) => {
                            let id = self.resolve_or_declare(name.as_str());
                            parts.reverse();
                            return Some((id, FieldPath(parts), *span));
                        }
                        _ => return None,
                    }
                }
            }
            ExpressionNode::Index(Spanned { node: boxed, span }) => {
                let mut parts: Vec<String> = vec!["*".to_string()];
                let mut cur: &ExpressionNode = &boxed.object;
                loop {
                    match cur {
                        ExpressionNode::Member(Spanned { node: inner, .. }) => {
                            parts.push(inner.property.name().to_string());
                            cur = &inner.object;
                        }
                        ExpressionNode::Index(Spanned { node: inner, .. }) => {
                            parts.push("*".to_string());
                            cur = &inner.object;
                        }
                        ExpressionNode::Identifier(Spanned {
                            node: IdentifierNode { name },
                            ..
                        }) => {
                            let id = self.resolve_or_declare(name.as_str());
                            parts.reverse();
                            return Some((id, FieldPath(parts), *span));
                        }
                        _ => return None,
                    }
                }
            }
            _ => None,
        }
    }

    fn borrow_immutable_id(&mut self, id: VarId, name: &str, span: Span) {
        // If any field is mutably borrowed, conservatively disallow immutable whole-var borrow
        if self.has_any_field_mut_borrow(id) {
            self.errors.push(format!(
                "cannot immutably borrow '{name}' while a field is mutably borrowed at line {} col {}",
                span.line, span.column
            ));
            return;
        }
        match self.get_state(id) {
            BorrowState::MutableBorrow => self.errors.push(format!(
                "cannot immutably borrow '{name}' while it is mutably borrowed at line {} col {}",
                span.line, span.column
            )),
            _ => self.set_state(id, BorrowState::ImmutableBorrow),
        }
    }

    fn borrow_mut_id(&mut self, id: VarId, name: &str, span: Span) {
        match self.get_state(id) {
            BorrowState::Free => self.set_state(id, BorrowState::MutableBorrow),
            BorrowState::ImmutableBorrow => self.errors.push(format!(
                "cannot mutably borrow '{name}' while it is immutably borrowed at line {} col {}",
                span.line, span.column
            )),
            BorrowState::MutableBorrow => self.errors.push(format!(
                "cannot create multiple mutable borrows of '{name}' at line {} col {}",
                span.line, span.column
            )),
        }
    }

    fn release_id(&mut self, id: VarId) {
        self.set_state(id, BorrowState::Free);
    }

    fn resolve_or_declare(&mut self, name: &str) -> VarId {
        if let Some(id) = self.sym.resolve(name) {
            id
        } else {
            self.declare(name)
        }
    }

    fn check_concurrency_sharing_args(&mut self, op: &str, args: &NodeList<ExpressionNode>) {
        for arg in args {
            if let Some((aid, _, aspan)) = self.resolve_lvalue(arg) {
                let mutably_borrowed = matches!(self.get_state(aid), BorrowState::MutableBorrow)
                    || self.has_any_field_mut_borrow(aid);
                if mutably_borrowed {
                    self.errors.push(format!(
                        "cannot share a mutably borrowed value across '{op}' at line {} col {}",
                        aspan.line, aspan.column
                    ));
                }

                // Privacy-aware sharing rule: do not allow PHI-like values across concurrency boundaries.
                if let ExpressionNode::Identifier(Spanned { node: ident, .. }) = arg {
                    if let Some(label) = self.privacy_labels.get(ident.name()) {
                        if !matches!(label, PrivacyAnnotation::Anonymized) {
                            self.errors.push(format!(
                                "cannot share {label:?} value '{}' across '{op}' at line {} col {}",
                                ident.name(),
                                aspan.line,
                                aspan.column
                            ));
                        }
                    }
                }
            }
        }
    }
}

impl Visitor for BorrowChecker {
    type Output = ();

    fn visit_program(&mut self, node: &ProgramNode) -> VisitResult<Self::Output> {
        self.enter_scope();
        self.enter_region();
        let r = self.visit_children(node);
        self.exit_region();
        self.exit_scope();
        r
    }

    fn visit_block(&mut self, node: &BlockNode) -> VisitResult<Self::Output> {
        self.enter_scope();
        self.enter_region();
        let r = self.visit_children(node);
        self.exit_region();
        self.exit_scope();
        r
    }

    fn visit_let_stmt(&mut self, node: &LetStatementNode) -> VisitResult<Self::Output> {
        let _id = self.declare(node.name.name());
        // Evaluate initializer for side-effects on borrow states
        if let Some(val) = &node.value {
            val.accept(self)?;
        }
        Ok(())
    }

    fn visit_assignment(&mut self, node: &AssignmentNode) -> VisitResult<Self::Output> {
        // Resolve lvalue (identifier or member chain)
        if let Some((var, path_opt, span)) = self.resolve_lvalue(&node.target) {
            self.enter_region();
            match path_opt {
                None => {
                    // Whole-variable mutable borrow
                    self.borrow_mut_id(var, "<var>", span);
                }
                Some(ref path) => {
                    // Partial mutable borrow on subpath
                    match self.get_field_state(var, path) {
                        BorrowState::Free => self.set_field_state(var, path.clone(), BorrowState::MutableBorrow),
                        BorrowState::ImmutableBorrow => self.errors.push(format!("cannot mutably borrow field while it is immutably borrowed at line {} col {}", span.line, span.column)),
                        BorrowState::MutableBorrow => self.errors.push(format!("cannot create multiple mutable borrows of field at line {} col {}", span.line, span.column)),
                    }
                }
            }
            node.value.accept(self)?;
            self.exit_region();
            // Release explicit borrow at end of assignment
            match path_opt {
                None => self.release_id(var),
                Some(path) => {
                    self.set_field_state(var, path, BorrowState::Free);
                }
            }
            Ok(())
        } else {
            self.visit_children(node)
        }
    }

    fn visit_call_expr(&mut self, node: &CallExpressionNode) -> VisitResult<Self::Output> {
        // By default, calls form a region; borrows inside do not leak out.
        // With feature "conservative_fn_ref", we do NOT isolate the call in its own region
        // to conservatively allow function-returned references to influence outer lifetime.
        #[cfg(not(feature = "conservative_fn_ref"))]
        {
            self.enter_region();
            if let ExpressionNode::Identifier(Spanned {
                node: IdentifierNode { name },
                span,
            }) = &node.callee
            {
                let s = name.as_str();
                let id = self.resolve_or_declare(s);
                self.borrow_immutable_id(id, s, *span);

                // Concurrency-aware borrowing (prototype): prohibit capturing/sharing values
                // that are currently mutably borrowed.
                if matches!(s, "spawn_task" | "spawn_task_with_priority" | "sched_spawn") {
                    self.check_concurrency_sharing_args(s, &node.arguments);
                }

                // Apply alias summary: borrow aliased argument immutably within the call
                if let Some(summary) = self.alias_summaries.get(s) {
                    if let Some(arg_idx) = summary.returns_ref_to_arg {
                        if let Some(arg) = node.arguments.get(arg_idx) {
                            if let Some((aid, _, aspan)) = self.resolve_lvalue(arg) {
                                self.borrow_immutable_id(aid, "<arg>", aspan);
                            }
                        }
                    }
                }
            } else {
                // Member-call concurrency (analytics pattern): tx.send(x) / tx.try_send(x)
                if let ExpressionNode::Member(Spanned { node: boxed, .. }) = &node.callee {
                    let meth = boxed.property.name();
                    if matches!(meth, "send" | "try_send") {
                        self.check_concurrency_sharing_args(meth, &node.arguments);
                    }
                }
                node.callee.accept(self)?;
            }
            for arg in &node.arguments {
                arg.accept(self)?;
            }
            self.exit_region();
            Ok(())
        }
        #[cfg(feature = "conservative_fn_ref")]
        {
            if let ExpressionNode::Identifier(Spanned {
                node: IdentifierNode { name },
                span,
            }) = &node.callee
            {
                let s = name.as_str();
                let id = self.resolve_or_declare(s);
                self.borrow_immutable_id(id, s, *span);
                if matches!(s, "spawn_task" | "spawn_task_with_priority" | "sched_spawn") {
                    self.check_concurrency_sharing_args(s, &node.arguments);
                }
                // Apply alias summary: borrow aliased argument immutably and allow it to persist
                if let Some(summary) = self.alias_summaries.get(s) {
                    if let Some(arg_idx) = summary.returns_ref_to_arg {
                        if let Some(arg) = node.arguments.get(arg_idx) {
                            if let Some((aid, _, aspan)) = self.resolve_lvalue(arg) {
                                self.borrow_immutable_id(aid, "<arg>", aspan);
                            }
                        }
                    }
                }
            } else {
                if let ExpressionNode::Member(Spanned { node: boxed, .. }) = &node.callee {
                    let meth = boxed.property.name();
                    if matches!(meth, "send" | "try_send") {
                        self.check_concurrency_sharing_args(meth, &node.arguments);
                    }
                }
                node.callee.accept(self)?;
            }
            for arg in &node.arguments {
                arg.accept(self)?;
            }
            Ok(())
        }
    }

    fn visit_identifier(&mut self, node: &IdentifierNode) -> VisitResult<Self::Output> {
        // Reading a variable: immutable borrow within current region
        let id = self.resolve_or_declare(node.name());
        self.borrow_immutable_id(id, node.name(), Span::default());
        Ok(())
    }

    fn visit_member_expr(&mut self, node: &MemberExpressionNode) -> VisitResult<Self::Output> {
        // Treat `a.b.c` as a field-path borrow on the base `a` and avoid treating
        // property identifiers as variable reads.
        let expr = ExpressionNode::Member(Spanned::new(Box::new(node.clone()), Span::default()));
        if let Some((id, path, span)) = self.resolve_access_path(&expr) {
            self.borrow_immutable_field(id, path, span);
            Ok(())
        } else {
            // Fallback: visit object only
            node.object.accept(self)
        }
    }

    fn visit_index_expr(&mut self, node: &IndexExpressionNode) -> VisitResult<Self::Output> {
        // Treat `a[i]` (and nested chains) as a field-path borrow on the base `a`.
        // Still evaluate index expression for borrows (e.g., reading `i`).
        let expr = ExpressionNode::Index(Spanned::new(Box::new(node.clone()), Span::default()));
        if let Some((id, path, span)) = self.resolve_access_path(&expr) {
            self.borrow_immutable_field(id, path, span);
        } else {
            node.object.accept(self)?;
        }
        node.index.accept(self)
    }

    fn visit_if_stmt(&mut self, node: &IfNode) -> VisitResult<Self::Output> {
        // Evaluate condition in isolated region (no leaking borrows)
        self.enter_region();
        node.condition.accept(self)?;
        self.exit_region();

        // Snapshot incoming state
        let saved_states = self.states.clone();
        let saved_fields = self.field_states.clone();

        // THEN branch: start from saved, record result
        self.states = saved_states.clone();
        self.field_states = saved_fields.clone();
        node.then_branch.accept(self)?;
        let then_states = self.states.clone();
        let then_fields = self.field_states.clone();

        // ELSE branch: start from saved, record result (or saved if absent)
        let (else_states, else_fields) = if let Some(else_branch) = &node.else_branch {
            self.states = saved_states.clone();
            self.field_states = saved_fields.clone();
            else_branch.accept(self)?;
            (self.states.clone(), self.field_states.clone())
        } else {
            (saved_states.clone(), saved_fields.clone())
        };

        // Merge results into current state
        self.states = self.merge_maps(&then_states, &else_states);
        self.field_states = self.merge_maps(&then_fields, &else_fields);

        Ok(())
    }

    fn visit_while_loop(&mut self, node: &WhileNode) -> VisitResult<Self::Output> {
        // Conservatively: condition and body each in isolated regions
        self.enter_region();
        node.condition.accept(self)?;
        self.exit_region();
        self.enter_region();
        node.body.accept(self)?;
        self.exit_region();
        Ok(())
    }
}
