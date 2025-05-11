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
    "#;
    let (_rest, stmts) = parse_program(code).unwrap();
    let mut env = TypeEnv::new();
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
    let mut env = TypeEnv::new();
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
    let mut env = TypeEnv::new();
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
    let mut env = TypeEnv::new();
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
    let mut env = TypeEnv::new();
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
    let mut env = TypeEnv::new();
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
