use medi_data::{
    fhir::FHIRObservation,
    fhir_query,
    sanitize::{
        canonize_observation_code, normalize_observation_unit, normalize_observation_unit_alias,
    },
};

#[test]
fn unit_aliases_normalize_to_canonical() {
    let mut o1 = FHIRObservation {
        id: "o1".into(),
        code: " hr ".into(),
        value: Some(70.0),
        unit: Some("beats per minute".into()),
    };
    let mut o2 = FHIRObservation {
        id: "o2".into(),
        code: "HR".into(),
        value: Some(72.0),
        unit: Some("beat/min".into()),
    };
    let mut o3 = FHIRObservation {
        id: "o3".into(),
        code: "TEMP".into(),
        value: Some(37.0),
        unit: Some("celsius".into()),
    };
    let mut o4 = FHIRObservation {
        id: "o4".into(),
        code: "BP".into(),
        value: Some(120.0),
        unit: Some("mm hg".into()),
    };

    for o in [&mut o1, &mut o2, &mut o3, &mut o4] {
        canonize_observation_code(o);
        normalize_observation_unit_alias(o);
        normalize_observation_unit(o);
    }

    assert_eq!(o1.unit.as_deref(), Some("bpm"));
    assert_eq!(o2.unit.as_deref(), Some("bpm"));
    assert_eq!(o3.unit.as_deref(), Some("c"));
    assert_eq!(o4.unit.as_deref(), Some("mmhg"));

    // Query for HR in bpm
    let obs = vec![o1, o2, o3, o4];
    let q = fhir_query("Observation")
        .filter_eq_ci("code", "HR")
        .filter_eq_ci("unit", "bpm")
        .build();
    let res = q.execute_observations(&obs);
    assert_eq!(res.len(), 2);
}
