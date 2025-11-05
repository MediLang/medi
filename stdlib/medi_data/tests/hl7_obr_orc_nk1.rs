use medi_data::hl7_fhir_more::{
    hl7_has_nk1_with_name, hl7_to_medical_event_from_orc, hl7_to_observations_from_obr,
};

#[test]
fn obr_to_observations_minimal() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\r\
               OBR|1|PLACER1|FILLER1|CBC^Complete Blood Count\r\
               OBR|2|||BMP^Basic Met Panel\r";
    let obs = hl7_to_observations_from_obr(hl7).expect("obs");
    assert_eq!(obs.len(), 2);
    assert_eq!(obs[0].id, "PLACER1");
    assert_eq!(obs[0].code, "CBC");
    assert_eq!(obs[1].id, "2"); // fallback to index
    assert_eq!(obs[1].code, "BMP");
}

#[test]
fn orc_to_medical_event_minimal() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\r\
               ORC|NW|PLACER2|FILLER2||||||202410151200\r";
    let evt = hl7_to_medical_event_from_orc(hl7).expect("evt");
    assert_eq!(evt.id, "PLACER2");
    assert_eq!(evt.code, "NW");
    assert_eq!(evt.start_date.as_deref(), Some("2024-10-15"));
}

#[test]
fn nk1_presence_detection() {
    let hl7_with_nk1 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r\
                       NK1|1|Doe^John\r";
    let hl7_without_nk1 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r";
    assert!(hl7_has_nk1_with_name(hl7_with_nk1));
    assert!(!hl7_has_nk1_with_name(hl7_without_nk1));
}
