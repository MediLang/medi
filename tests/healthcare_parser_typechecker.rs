use medi::parser::{parse_program, StatementNode};
use medi::type_checker::{DefaultTypeChecker, TypeEnv, TypeError};
use medi::types::MediType;
use medi::env::TypeEnv;
use std::collections::HashMap;

#[test]
fn test_patient_record_declaration_valid() {
    let code = r#"
        patient record Patient {
            id: Int,
            name: String,
            age: Int
        }

#[test]
fn test_clinical_workflow_med_interaction_gate() {
    // End-to-end: parse program that uses med_interact in a condition and assigns alert
    // Note: DefaultTypeChecker does not attach a ValidationCtx here; the goal is E2E flow.
    let code = r#"
        clinical rule MedCheck {
            when: med_interact("acetaminophen", "ibuprofen"),
            then: {
                alert = "Check regimen";
            }
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    env.insert("alert".to_string(), medi::types::MediType::String);
    env.insert("String".to_string(), medi::types::MediType::String);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        assert!(checker.check_stmt(stmt).is_ok());
    }
}

#[test]
fn test_clinical_workflow_record_ops_and_rule() {
    // End-to-end: combine record operations with a clinical rule using a numeric comparison
    let code = r#"
        let rec = r1 | r2 & r1; // record ops
        clinical rule HighRisk {
            when: risk_score > 7,
            then: {
                alert = "High risk patient";
            }
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    // Provide environment bindings used by the code
    env.insert("r1".to_string(), medi::types::MediType::MedicalRecord);
    env.insert("r2".to_string(), medi::types::MediType::MedicalRecord);
    env.insert("risk_score".to_string(), medi::types::MediType::Int);
    env.insert("alert".to_string(), medi::types::MediType::String);
    env.insert("String".to_string(), medi::types::MediType::String);
    env.insert("Int".to_string(), medi::types::MediType::Int);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        assert!(checker.check_stmt(stmt).is_ok());
    }
    // Ensure rec inferred type is MedicalRecord
    let rec_ty = checker.env().get("rec").cloned().expect("rec bound");
    assert_eq!(rec_ty, medi::types::MediType::MedicalRecord);
}

#[test]
fn test_record_pipelines_plus_union_intersection() {
    // Prepare env with two medical records
    let mut env = TypeEnv::with_prelude();
    env.insert("r1".to_string(), medi::types::MediType::MedicalRecord);
    env.insert("r2".to_string(), medi::types::MediType::MedicalRecord);

    // Mix +, |, & in a pipeline; should type to MedicalRecord without errors
    let code = r#"
        let r = r1 + r2 | r1 & r2;
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        if let Err(e) = checker.check_stmt(stmt) {
            panic!("type error: {e}");
        }
    }
    // r should be MedicalRecord
    let r_ty = checker.env().get("r").cloned().expect("r bound");
    assert_eq!(r_ty, medi::types::MediType::MedicalRecord);
}

#[test]
fn test_quantity_of_per_assignment_and_mismatch() {
    // Create quantities via of/per and verify assignability/mismatch
    let code = r#"
        let q = 5 of 7;        // Quantity(Int)
        let rate = 2.0 per 3;  // Quantity(Float)
        q = 1;                 // mismatch: Quantity(Int) vs Int
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut checker = DefaultTypeChecker::new();

    let mut saw_mismatch = false;
    for stmt in &stmts {
        match checker.check_stmt(stmt) {
            Ok(()) => {}
            Err(msg) => {
                // Expect a type mismatch when assigning Int to Quantity(Int)
                if msg.contains("Type mismatch") {
                    saw_mismatch = true;
                } else {
                    panic!("unexpected error: {msg}");
                }
            }
        }
    }
    assert!(saw_mismatch, "expected a mismatch assigning Int to Quantity(Int)");

    // Verify inferred env types for q and rate are quantities
    let env = checker.env();
    let q_ty = env.get("q").cloned().expect("q bound");
    let rate_ty = env.get("rate").cloned().expect("rate bound");
    match q_ty { medi::types::MediType::Quantity(inner) => assert_eq!(*inner, medi::types::MediType::Int), other => panic!("expected Quantity(Int), got {other:?}") }
    match rate_ty { medi::types::MediType::Quantity(inner) => assert_eq!(*inner, medi::types::MediType::Float), other => panic!("expected Quantity(Float), got {other:?}") }
}

#[test]
fn test_domain_errors_invalid_of_per_and_record_set_mismatch() {
    // Non-numeric of/per should produce domain-aware errors.
    // Record set op mismatches should emit domain error as well.
    let code = r#"
        let badq = "x" of 5;
        let rec = r | 1; // r will be provided from env as MedicalRecord
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();

    let mut env = TypeEnv::with_prelude();
    env.insert("r".to_string(), medi::types::MediType::MedicalRecord);
    let mut checker = DefaultTypeChecker::new_with_env(env);

    let mut found_domain_err = 0;
    for stmt in &stmts {
        if let Err(msg) = checker.check_stmt(stmt) {
            if msg.contains("Medical operator expects numeric operands") || msg.contains("Medical record set ops") {
                found_domain_err += 1;
            }
        }
    }
    assert!(found_domain_err >= 2, "expected at least two domain errors, found {found_domain_err}");
}
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    env.insert("Int".to_string(), medi::types::MediType::Int);
    env.insert("String".to_string(), medi::types::MediType::String);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        assert!(checker.check_stmt(stmt).is_ok());
    }
}

#[test]
fn test_patient_record_declaration_invalid_type() {
    let code = r#"
        patient_record P1 { 
            age: UnknownType, 
            name: String 
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    env.insert("Int".to_string(), medi::types::MediType::Int);
    env.insert("String".to_string(), medi::types::MediType::String);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    let mut found_error = false;
    for stmt in &stmts {
        if let Err(msg) = checker.check_stmt(stmt) {
            assert!(msg.contains("Unknown type"));
            found_error = true;
        }
    }
    assert!(found_error);
}


#[test]
fn test_clinical_rule_valid() {
    let code = r#"
        clinical rule HighBP {
            when: patient.bp > 140,
            then: {
                alert = "High blood pressure";
            }
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    let mut patient_fields = HashMap::new();
    patient_fields.insert("bp".to_string(), medi::types::MediType::Int);
    env.insert("patient".to_string(), medi::types::MediType::Struct(patient_fields));
    env.insert("alert".to_string(), medi::types::MediType::String);
    env.insert("String".to_string(), medi::types::MediType::String);
    env.insert("Int".to_string(), medi::types::MediType::Int);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        assert!(checker.check_stmt(stmt).is_ok());
    }
}

#[test]
fn test_clinical_rule_invalid_condition() {
    let code = r#"
        clinical rule BadRule {
            when: patient.name,
            then: {
                alert = "Bad";
            }
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    env.insert("patient".to_string(), medi::types::MediType::Struct(Default::default()));
    env.insert("alert".to_string(), medi::types::MediType::String);
    env.insert("String".to_string(), medi::types::MediType::String);
    let mut checker = DefaultTypeChecker::new_with_env(env);
    let mut found_error = false;
    for stmt in &stmts {
        if let Err(msg) = checker.check_stmt(stmt) {
            assert!(msg.contains("condition must be Bool"));
            found_error = true;
        }
    }
    assert!(found_error);
}

#[test]
fn test_medical_transformation_valid() {
    let code = r#"
        medical_transformation NormalizeBP(patient: Int) -> PlaceholderReturnType {
            // ...logic
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::with_prelude();
    env.insert("patient".to_string(), medi::types::MediType::Int);
    env.insert("NormalizeBP".to_string(), medi::types::MediType::Function { params: vec![medi::types::MediType::Int], return_type: Box::new(medi::types::MediType::Int) });
    let mut checker = DefaultTypeChecker::new_with_env(env);
    for stmt in &stmts {
        assert!(checker.check_stmt(stmt).is_ok());
    }
}

#[test]
fn test_medical_transformation_wrong_input() {
    // DEBUG: This test should fail if the transformation input type is wrong

    let code = r#"
        medical_transformation NormalizeBP(patient: UnknownType) -> PlaceholderReturnType {
            // ...logic
        }
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    println!("[DEBUG] AST for medical transformation wrong input: {:#?}", stmts);
    let mut env = TypeEnv::with_prelude();
    env.insert("patient".to_string(), medi::types::MediType::String);
    env.insert("NormalizeBP".to_string(), medi::types::MediType::Function { params: vec![medi::types::MediType::Int], return_type: Box::new(medi::types::MediType::Int) });
    let mut type_checker = DefaultTypeChecker::new_with_env(env);
    let errors = type_checker.check_program(&stmts); // stmts is ProgramNode here from parse_program

    let mut found_correct_error = false;
    if errors.is_empty() {
        println!("No type errors found by check_program.");
    }
    for err_enum in &errors { 
        let msg = format!("{}", err_enum); 
        println!("Type Error: {}", msg); 
        // Check the specific message for UnknownType
        if msg.contains("Type 'UnknownType' is undefined or has an unknown type.") {
            // Optionally, also check the enum variant and name if needed for super robustness
            if let TypeError::UnknownType(name) = err_enum {
                if name == "UnknownType" {
                    found_correct_error = true;
                    break; 
                }
            }
        }
    }
    assert!(found_correct_error, "Expected 'UnknownType' error not found or message mismatch. Errors: {:?}", errors);

}
