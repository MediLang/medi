use tolvex_data::{
    fhir_query,
    sanitize::{canonize_observation_code, normalize_observation_unit},
};

#[path = "util/fixtures.rs"]
mod fixtures;

#[test]
fn sanitize_then_query_observations() {
    let mut obs = fixtures::load_observations();

    // sanitize all
    for o in &mut obs {
        canonize_observation_code(o);
        normalize_observation_unit(o);
    }

    // Query: code == HR and unit == bpm
    let q = fhir_query("Observation")
        .filter_eq_ci("code", "HR")
        .filter_eq_ci("unit", "bpm")
        .build();
    let res = q.execute_observations(&obs);

    // Expect o1, o2, and o4 after normalization; but o4 unit becomes "beats/min"? We normalized to lowercase trim; keep exact "beats/min" won't match eq "bpm".
    // Given normalization is lowercase+trim only, expect o1 and o2 to match (" BPM "->"bpm", "bpm"->"bpm"), o4 won't match.
    assert_eq!(res.len(), 2);
    assert!(res.iter().any(|o| o.id == "o1"));
    assert!(res.iter().any(|o| o.id == "o2"));
}
