use tolvex_data::hl7::{parse_field_components, parse_hl7};

#[test]
fn parse_msh_segment_extracts_separators() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.field_sep, '|');
    assert_eq!(parsed.component_sep, '^');
    assert_eq!(parsed.repetition_sep, '~');
    assert_eq!(parsed.escape_char, '\\');
    assert_eq!(parsed.subcomponent_sep, '&');
}

#[test]
fn parse_msh_segment_fields() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 1);
    let msh = &parsed.segments[0];
    assert_eq!(msh.name, "MSH");
    assert_eq!(msh.fields[0], "|");
    assert_eq!(msh.fields[1], "^~\\&");
    assert_eq!(msh.fields[2], "SRC");
    assert_eq!(msh.fields[3], "FAC");
}

#[test]
fn parse_adt_a01_message() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\rPID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    assert_eq!(parsed.segments[0].name, "MSH");
    assert_eq!(parsed.segments[1].name, "PID");
    let pid = &parsed.segments[1];
    assert!(pid.fields.len() >= 6, "PID fields: {:?}", pid.fields);
    assert_eq!(pid.fields[0], "1");
    assert_eq!(pid.fields[1], "ALTID");
    assert_eq!(pid.fields[2], "P12345");
}

#[test]
fn parse_message_with_multiple_segments() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\rPID|1|ALTID|P12345||Doe^Jane||19851224|F\rOBX|1|NM|HR||72|bpm|60-100|N||F\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 3);
    assert_eq!(parsed.segments[0].name, "MSH");
    assert_eq!(parsed.segments[1].name, "PID");
    assert_eq!(parsed.segments[2].name, "OBX");
}

#[test]
fn parse_obx_segment_with_numeric_value() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORU^R01|123|P|2.5\rOBX|1|NM|HR^Heart Rate||72|bpm|60-100|N||F\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    let obx = &parsed.segments[1];
    assert_eq!(obx.name, "OBX");
    assert_eq!(obx.fields[0], "1");
    assert_eq!(obx.fields[1], "NM");
    assert_eq!(obx.fields[2], "HR^Heart Rate");
    assert_eq!(obx.fields[4], "72");
    assert_eq!(obx.fields[5], "bpm");
}

#[test]
fn parse_field_components_with_repetitions() {
    let field = "A^B&C~D^E";
    let parsed = parse_field_components(field);
    assert_eq!(parsed.len(), 2);
    assert_eq!(parsed[0].len(), 2);
    assert_eq!(parsed[0][0].len(), 1);
    assert_eq!(parsed[0][0][0], "A");
    assert_eq!(parsed[0][1].len(), 2);
    assert_eq!(parsed[0][1][0], "B");
    assert_eq!(parsed[0][1][1], "C");
    assert_eq!(parsed[1].len(), 2);
    assert_eq!(parsed[1][0][0], "D");
    assert_eq!(parsed[1][1][0], "E");
}

#[test]
fn parse_empty_message() {
    let msg = "";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 0);
}

#[test]
fn parse_msh_only() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 1);
    assert_eq!(parsed.segments[0].name, "MSH");
}

#[test]
fn parse_non_msh_start_fails() {
    let msg = "PID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
    let result = parse_hl7(msg);
    assert!(result.is_err());
}

#[test]
fn parse_message_with_crlf_separators() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ADT^A01|123|P|2.5\rPID|1|ALTID|P12345||Doe^Jane||19851224|F\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    assert_eq!(parsed.segments[0].name, "MSH");
    assert_eq!(parsed.segments[1].name, "PID");
}

#[test]
fn parse_orc_segment() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORM^O01|123|P|2.5\rORC|NW|ORDER123|FILLER456|OK|CM|P|1|202501010800\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    let orc = &parsed.segments[1];
    assert_eq!(orc.name, "ORC");
    assert_eq!(orc.fields[0], "NW");
    assert_eq!(orc.fields[1], "ORDER123");
    assert_eq!(orc.fields[2], "FILLER456");
}

#[test]
fn parse_obr_segment() {
    let msg = "MSH|^~\\&|SRC|FAC|DST|HOSP|202501010101||ORU^R01|123|P|2.5\rOBR|1|ORDER123|FILLER456|85025^CBC^LN||202501010800\r";
    let parsed = parse_hl7(msg).expect("parse");
    assert_eq!(parsed.segments.len(), 2);
    let obr = &parsed.segments[1];
    assert_eq!(obr.name, "OBR");
    assert_eq!(obr.fields[0], "1");
    assert_eq!(obr.fields[1], "ORDER123");
    assert_eq!(obr.fields[2], "FILLER456");
    assert_eq!(obr.fields[3], "85025^CBC^LN");
}
