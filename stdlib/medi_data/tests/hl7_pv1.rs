use medi_data::hl7_fhir::hl7_to_medical_event_minimal;

#[test]
fn pv1_to_medical_event_minimal() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
               PID|1|ALTID|P12345||Doe^Jane||19851224|F\r\
               PV1|1|I|W^101^1^HOSP|||^^^^|^^^^|^^^^|CARD|||||||||VN12345||||||||||||||||20241015";
    let evt = hl7_to_medical_event_minimal(hl7).expect("evt");
    assert_eq!(evt.id, "VN12345");
    assert_eq!(evt.code, "CARD");
    assert_eq!(evt.start_date.as_deref(), Some("2024-10-15"));
}
