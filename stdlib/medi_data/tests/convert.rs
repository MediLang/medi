use medi_data::convert::{diagnostic_report_from_observation_codes, link_conditions_to_encounters};
use medi_data::fhir::*;

#[test]
fn link_conditions_to_encounters_by_subject() {
    let conditions = vec![
        FHIRCondition {
            id: "c1".into(),
            code: "COND".into(),
            clinical_status: "active".into(),
            verification_status: "confirmed".into(),
            subject: "pat1".into(),
        },
        FHIRCondition {
            id: "c2".into(),
            code: "COND".into(),
            clinical_status: "active".into(),
            verification_status: "confirmed".into(),
            subject: "pat2".into(),
        },
    ];
    let encounters = vec![FHIREncounter {
        id: "e1".into(),
        class_: None,
        start_date: None,
        end_date: None,
        subject: "pat1".into(),
    }];
    let linked = link_conditions_to_encounters(&conditions, &encounters);
    assert_eq!(linked.len(), 2);
    assert_eq!(linked[0].0.id, "c1");
    assert!(linked[0].1.as_ref().is_some());
    assert_eq!(linked[0].1.as_ref().unwrap().id, "e1");
    assert_eq!(linked[1].0.id, "c2");
    assert!(linked[1].1.is_none());
}

#[test]
fn build_diagnostic_report_from_codes() {
    let rep = diagnostic_report_from_observation_codes("r1", "pat1", "DRPT", &["OBS1", "OBS2"]);
    assert_eq!(rep.id, "r1");
    assert_eq!(rep.subject, "pat1");
    assert_eq!(rep.code, "DRPT");
    assert_eq!(rep.result_codes, vec!["OBS1", "OBS2"]);
}
