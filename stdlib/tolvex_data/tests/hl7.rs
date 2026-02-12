use tolvex_data::hl7::parse_hl7;

#[test]
fn parse_basic_hl7_segments() {
    let msg = "MSH|^~\\&|ACME|LAB|CLINIC|HOSP|202501010101||ORU^R01|123|P|2.5\rPID|1||123456^^^CLINIC^MR||Doe^John||19800101|M\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    assert_eq!(parsed.segments[0].name, "MSH");
    assert!(!parsed.segments[0].fields.is_empty());
    assert_eq!(parsed.segments[1].name, "PID");
}
