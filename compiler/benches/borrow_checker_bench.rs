use criterion::{black_box, criterion_group, criterion_main, Criterion};
use tlvxc_ast::ast::*;
use tlvxc_ast::visit::Span;
use tlvxc_borrowck::BorrowChecker;

fn build_program(num_stmts: usize) -> ProgramNode {
    // Construct a program with a sequence of let/assign/expr patterns to simulate ~num_stmts scale
    let mut stmts: Vec<StatementNode> = Vec::with_capacity(num_stmts);
    // Seed variable
    stmts.push(StatementNode::Let(Box::new(LetStatementNode {
        name: IdentifierNode::from_str_name("x0"),
        type_annotation: None,
        value: Some(ExpressionNode::Literal(Spanned::new(
            LiteralNode::Int(0),
            Span::default(),
        ))),
        span: Span::default(),
    })));
    for i in 1..num_stmts {
        // let xi = i; xi = xi; expr xi
        let name = format!("x{i}");
        let prev = format!("x{}", i - 1);
        stmts.push(StatementNode::Let(Box::new(LetStatementNode {
            name: IdentifierNode::from_str_name(&name),
            type_annotation: None,
            value: Some(ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name(&prev),
                Span::default(),
            ))),
            span: Span::default(),
        })));
        stmts.push(StatementNode::Assignment(Box::new(AssignmentNode {
            target: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name(&name),
                Span::default(),
            )),
            value: ExpressionNode::Identifier(Spanned::new(
                IdentifierNode::from_str_name(&name),
                Span::default(),
            )),
            span: Span::default(),
        })));
        stmts.push(StatementNode::Expr(ExpressionNode::Identifier(
            Spanned::new(IdentifierNode::from_str_name(&name), Span::default()),
        )));
    }
    ProgramNode { statements: stmts }
}

fn bench_borrow_checker(c: &mut Criterion) {
    // Target scale roughly approximating < 10k LOC; we use 5000 statements as a proxy
    let program = build_program(5_000);
    c.bench_function("borrow_checker_5k_stmts", |b| {
        b.iter(|| {
            let mut chk = BorrowChecker::new();
            let res = chk.check_program(black_box(&program));
            // We just ensure it runs; errors may exist due to x=x patterns in gen
            let _ = res.is_ok();
        })
    });
}

criterion_group!(benches, bench_borrow_checker);
criterion_main!(benches);
