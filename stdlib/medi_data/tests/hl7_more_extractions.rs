use medi_data::hl7_fhir_more::{hl7_extract_nk1_contacts, hl7_obr_dates_minimal};

#[test]
fn obr_dates_and_nk1_contacts() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\r\
               OBR|1|PLACER1|FILLER1|CBC^Complete Blood Count||||202410151200\r\
               NK1|1|Doe^Jane\r";
    let dates = hl7_obr_dates_minimal(hl7);
    assert_eq!(dates, vec!["2024-10-15".to_string()]);
    let contacts = hl7_extract_nk1_contacts(hl7);
    assert_eq!(contacts, vec![("Doe".into(), "Jane".into())]);
}
