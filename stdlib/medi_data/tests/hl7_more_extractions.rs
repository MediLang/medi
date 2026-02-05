use medi_data::hl7::parse_hl7;
use medi_data::hl7_fhir_more::{hl7_extract_nk1_contacts, hl7_obr_dates_minimal};

#[test]
fn obr_dates_and_nk1_contacts() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\rOBR|1|PLACER1|FILLER1|CBC^Complete Blood Count||||202410151200\rNK1|1|Doe^Jane";

    let parsed = parse_hl7(hl7).expect("parse failed");
    assert!(
        parsed.segments.len() >= 2,
        "Expected at least 2 segments, got {}",
        parsed.segments.len()
    );

    let obr_seg = parsed
        .segments
        .iter()
        .find(|s| s.name == "OBR")
        .expect("OBR segment not found");
    assert!(
        obr_seg.fields.len() >= 8,
        "OBR has {} fields, expected at least 8",
        obr_seg.fields.len()
    );
    assert_eq!(
        obr_seg.fields[7], "202410151200",
        "OBR field 7 should be the date"
    );

    let dates = hl7_obr_dates_minimal(hl7);
    assert_eq!(dates, vec!["2024-10-15".to_string()]);
    let contacts = hl7_extract_nk1_contacts(hl7);
    assert_eq!(contacts, vec![("Doe".into(), "Jane".into())]);
}
