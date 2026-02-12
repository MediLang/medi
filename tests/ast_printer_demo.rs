use tlvxc::ast::*;

#[test]
fn print_sample_healthcare_ast() {
    let patient_decl = StatementNode::PatientRecordDeclaration(Box::new(
        PatientRecordDeclarationNode {
            metadata: None,
            name: "Patient".to_string(),
            fields: vec![
                ("id".to_string(), "Int".to_string()),
                ("name".to_string(), "String".to_string()),
                ("age".to_string(), "Int".to_string()),
            ],
        },
    ));

    let clinical_rule = StatementNode::ClinicalRule(Box::new(
        ClinicalRuleNode {
            priority: None,
            metadata: None,
            name: "CheckHypertension".to_string(),
            condition: ExpressionNode::Binary(Box::new(BinaryExpressionNode {
                left: ExpressionNode::Member(Box::new(MemberExpressionNode {
                    object: ExpressionNode::Identifier("patient".to_string()),
                    property: "age".to_string(),
                })),
                operator: BinaryOperator::Gt,
                right: ExpressionNode::Literal(LiteralNode::Int(65)),
            })),
            action: BlockNode {
                statements: vec![StatementNode::Expr(ExpressionNode::Call(Box::new(
                    CallExpressionNode {
                        callee: ExpressionNode::Identifier("alert_doctor".to_string()),
                        arguments: vec![ExpressionNode::Identifier("patient".to_string())],
                    },
                )))],
            },
        },
    ));

    let transformation = StatementNode::MedicalTransformation(Box::new(
        MedicalTransformationNode {
            metadata: None,
            input: ExpressionNode::Identifier("bp_reading".to_string()),
            transformation: "normalize_blood_pressure".to_string(),
        },
    ));

    let program = ProgramNode {
        statements: vec![patient_decl, clinical_rule, transformation],
    };

    let mut printer = AstPrinter::new();
    program.accept(&mut printer);
}
