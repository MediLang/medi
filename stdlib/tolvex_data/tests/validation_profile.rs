use tolvex_data::fhir::FHIRPatient;
use tolvex_data::fhir_any::FHIRAny;
use tolvex_data::validate::ValidationErrorKind;
use tolvex_data::validation_profile::{
    load_profiles_from_str, validate_with_profile_any, validate_with_profile_json,
    ValidationProfile,
};
use serde_json::json;

#[test]
fn validate_json_required_fields_and_cardinality() {
    // Use DiagnosticReport for array cardinality on result_codes
    let prof = ValidationProfile {
        name: "BasicReport".to_string(),
        resource_type: "DiagnosticReport".to_string(),
        required_fields: vec!["id".to_string(), "code".to_string()],
        cardinality: [("result_codes".to_string(), (1, Some(3)))]
            .into_iter()
            .collect(),
    };

    // Valid JSON object
    let ok = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": ["A"]
    });
    assert!(validate_with_profile_json(&ok, &prof).is_ok());

    // Missing required field
    let missing = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "result_codes": ["A"]
    });
    assert!(validate_with_profile_json(&missing, &prof).is_err());

    // Cardinality min fail
    let too_few = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": []
    });
    assert!(validate_with_profile_json(&too_few, &prof).is_err());

    // Cardinality max fail
    let too_many = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": ["A","B","C","D"]
    });
    assert!(validate_with_profile_json(&too_many, &prof).is_err());
}

#[test]
fn validate_any_type_mismatch_and_success() {
    // Patient profile requiring id and given_name
    let prof = ValidationProfile {
        name: "BasicPatient".to_string(),
        resource_type: "Patient".to_string(),
        required_fields: vec!["id".to_string(), "given_name".to_string()],
        cardinality: Default::default(),
    };

    let p = FHIRPatient {
        id: "p2".to_string(),
        given_name: Some("John".to_string()),
        family_name: None,
        birth_date: None,
    };
    let any = FHIRAny::Patient(p);
    assert!(validate_with_profile_any(&any, &prof).is_ok());

    // Observation should fail type check
    let obs_json = json!({
        "resourceType": "Observation",
        "id": "o1",
        "code": "abc"
    });
    // Convert to JSON path to trigger type mismatch quickly
    assert!(validate_with_profile_json(&obs_json, &prof).is_err());
}

#[test]
fn typed_errors_are_emitted_for_profile_violations() {
    let prof = ValidationProfile {
        name: "BasicReport".to_string(),
        resource_type: "DiagnosticReport".to_string(),
        required_fields: vec!["id".to_string(), "code".to_string()],
        cardinality: [("result_codes".to_string(), (1, Some(2)))]
            .into_iter()
            .collect(),
    };

    // MissingField
    let missing = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1"
    });
    let err = validate_with_profile_json(&missing, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::MissingField { field } => assert_eq!(field, "code"),
        other => panic!("expected MissingField, got {other:?}"),
    }

    // EmptyField
    let empty = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "   ",
        "result_codes": ["A"]
    });
    let err = validate_with_profile_json(&empty, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::EmptyField { field } => assert_eq!(field, "code"),
        other => panic!("expected EmptyField, got {other:?}"),
    }

    // WrongType for result_codes not an array
    let wrong_type = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": "A"
    });
    let err = validate_with_profile_json(&wrong_type, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::WrongType { field, expected } => {
            assert_eq!(field, "result_codes");
            assert_eq!(expected, "array");
        }
        other => panic!("expected WrongType, got {other:?}"),
    }

    // Cardinality min
    let too_few = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": []
    });
    let err = validate_with_profile_json(&too_few, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::Cardinality {
            field,
            len,
            min,
            max,
        } => {
            assert_eq!(field, "result_codes");
            assert_eq!(len, 0);
            assert_eq!(min, 1);
            assert_eq!(max, Some(2));
        }
        other => panic!("expected Cardinality, got {other:?}"),
    }

    // Cardinality max
    let too_many = json!({
        "resourceType": "DiagnosticReport",
        "id": "r1",
        "code": "CMP",
        "result_codes": ["A","B","C"]
    });
    let err = validate_with_profile_json(&too_many, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::Cardinality {
            field,
            len,
            min,
            max,
        } => {
            assert_eq!(field, "result_codes");
            assert_eq!(len, 3);
            assert_eq!(min, 1);
            assert_eq!(max, Some(2));
        }
        other => panic!("expected Cardinality, got {other:?}"),
    }

    // TypeMismatch
    let wrong_type_resource = json!({
        "resourceType": "Observation",
        "id": "o1",
        "code": "CMP"
    });
    let err = validate_with_profile_json(&wrong_type_resource, &prof).unwrap_err();
    match err.kind {
        ValidationErrorKind::TypeMismatch { expected, found } => {
            assert_eq!(expected, "DiagnosticReport");
            assert_eq!(found, "Observation");
        }
        other => panic!("expected TypeMismatch, got {other:?}"),
    }
}

#[test]
fn load_profiles_from_json_array() {
    let json_profiles = r#"
    [
      {
        "name": "BasicPatient",
        "resource_type": "Patient",
        "required_fields": ["id", "name"],
        "cardinality": {"identifiers": [1, 3]}
      },
      {
        "name": "BasicObservation",
        "resource_type": "Observation",
        "required_fields": ["id", "code"],
        "cardinality": {}
      }
    ]
    "#;

    let profiles = load_profiles_from_str(json_profiles).expect("profiles parse");
    assert_eq!(profiles.len(), 2);
    assert_eq!(profiles[0].name, "BasicPatient");
    assert_eq!(profiles[1].resource_type, "Observation");
}
