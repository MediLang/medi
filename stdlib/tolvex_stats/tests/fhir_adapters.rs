#![cfg(feature = "fhir")]

use tolvex_data::fhir::FHIRObservation;
use tolvex_stats::extract_series_by_code;

#[test]
fn extract_series_filters_by_code_and_value() {
    let obs = vec![
        FHIRObservation {
            id: "1".into(),
            code: "BP".into(),
            value: Some(120.0),
            unit: Some("mmHg".into()),
        },
        FHIRObservation {
            id: "2".into(),
            code: "BP".into(),
            value: Some(130.0),
            unit: Some("mmHg".into()),
        },
        FHIRObservation {
            id: "3".into(),
            code: "HR".into(),
            value: Some(80.0),
            unit: Some("bpm".into()),
        },
        FHIRObservation {
            id: "4".into(),
            code: "BP".into(),
            value: None,
            unit: Some("mmHg".into()),
        },
        FHIRObservation {
            id: "5".into(),
            code: "BP".into(),
            value: Some(f64::NAN),
            unit: Some("mmHg".into()),
        },
    ];

    let series = extract_series_by_code(&obs, "BP");
    assert_eq!(series.len(), 2);
    assert!(series.contains(&120.0));
    assert!(series.contains(&130.0));
}
