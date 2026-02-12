use tolvex_data::hl7_fhir_more::hl7_observations_from_obr_with_time;

#[test]
fn obr_observations_with_time() {
    let hl7 = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\r\
               OBR|1|PLACER1|FILLER1|CBC^Complete Blood Count||||202410151230\r\
               OBR|2|||BMP^Basic Met Panel||||202410160000\r";
    let v = hl7_observations_from_obr_with_time(hl7).expect("vec");
    assert_eq!(v.len(), 2);
    assert_eq!(v[0].0.id, "PLACER1");
    assert_eq!(v[0].0.code, "CBC");
    assert_eq!(v[0].1.as_deref(), Some("2024-10-15"));
    assert_eq!(v[1].0.id, "2");
    assert_eq!(v[1].0.code, "BMP");
    assert_eq!(v[1].1.as_deref(), Some("2024-10-16"));
}
