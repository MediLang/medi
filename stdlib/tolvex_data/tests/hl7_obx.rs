use tolvex_data::hl7_fhir_obx::hl7_to_observations_minimal;

#[test]
fn obx_to_observations_minimal() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORU^R01|123|P|2.5\r\
               PID|1|ALTID|P12345||Doe^Jane||19851224|F\r\
               OBX|1|NM|HR^Heart Rate||70|bpm\r\
               OBX|2|ST|NOTE^Note||Normal||";
    let obs = hl7_to_observations_minimal(hl7).expect("obs");
    assert_eq!(obs.len(), 2);
    assert_eq!(obs[0].id, "1");
    assert_eq!(obs[0].code, "HR");
    assert_eq!(obs[0].value, Some(70.0));
    assert_eq!(obs[0].unit.as_deref(), Some("bpm"));
    assert_eq!(obs[1].id, "2");
    assert_eq!(obs[1].code, "NOTE");
    assert_eq!(obs[1].value, None);
}
