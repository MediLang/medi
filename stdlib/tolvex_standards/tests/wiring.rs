use tolvex_standards::{
    dicom::parse_dicom, fhir::FHIRPatient, hl7_fhir::hl7_to_fhir_patient_minimal,
};

#[test]
fn hl7_to_fhir_patient_minimal_round_trip() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\rPID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
    let p = hl7_to_fhir_patient_minimal(hl7).expect("patient");
    assert_eq!(p.id, "P12345");
    assert_eq!(p.family_name.as_deref(), Some("Doe"));
    assert_eq!(p.given_name.as_deref(), Some("Jane"));
    assert_eq!(p.birth_date.as_deref(), Some("1985-12-24"));

    let _ = FHIRPatient { ..p };
}

#[test]
fn dicom_magic_is_enforced() {
    // Minimal plausible DICOM: 128 preamble + 'DICM' + tag group/element + VR bytes
    let mut bytes = vec![0u8; 128];
    bytes.extend_from_slice(b"DICM");
    // group, element
    bytes.extend_from_slice(&1u16.to_le_bytes());
    bytes.extend_from_slice(&2u16.to_le_bytes());
    // VR
    bytes.extend_from_slice(b"PN");
    // pad to >= 140
    bytes.resize(140, 0);

    let obj = parse_dicom(&bytes).expect("dicom");
    assert!(obj.magic_ok);
}
