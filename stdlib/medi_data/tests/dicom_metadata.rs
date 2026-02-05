use medi_data::dicom::parse_dicom;

fn create_dicom_with_tags(tags: Vec<(u16, u16, &str, Vec<u8>)>) -> Vec<u8> {
    let mut bytes = vec![0u8; 128];
    bytes.extend_from_slice(b"DICM");

    for (group, element, vr, value) in tags {
        bytes.extend_from_slice(&group.to_le_bytes());
        bytes.extend_from_slice(&element.to_le_bytes());
        bytes.extend_from_slice(vr.as_bytes());

        match vr {
            "OB" | "OD" | "OF" | "OL" | "OW" | "UN" | "UC" | "UR" | "UT" => {
                bytes.extend_from_slice(&[0u8, 0u8]);
                bytes.extend_from_slice(&(value.len() as u32).to_le_bytes());
            }
            _ => {
                bytes.extend_from_slice(&(value.len() as u16).to_le_bytes());
            }
        }

        bytes.extend_from_slice(&value);
    }

    bytes
}

#[test]
fn parse_dicom_with_patient_name_tag() {
    let tags = vec![(0x0010, 0x0010, "PN", b"Doe^Jane".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert!(obj.has_preamble);
    assert!(obj.magic_ok);
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x0010);
    assert_eq!(tag.element, 0x0010);
    assert_eq!(tag.vr, "PN");
    assert_eq!(tag.value_length, 8);
    assert_eq!(tag.value, b"Doe^Jane");
}

#[test]
fn parse_dicom_with_patient_id_tag() {
    let tags = vec![(0x0010, 0x0020, "LO", b"P12345".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x0010);
    assert_eq!(tag.element, 0x0020);
    assert_eq!(tag.vr, "LO");
    assert_eq!(tag.value, b"P12345");
}

#[test]
fn parse_dicom_with_study_date_tag() {
    let tags = vec![(0x0008, 0x0020, "DA", b"20250205".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x0008);
    assert_eq!(tag.element, 0x0020);
    assert_eq!(tag.vr, "DA");
    assert_eq!(tag.value, b"20250205");
}

#[test]
fn parse_dicom_with_modality_tag() {
    let tags = vec![(0x0008, 0x0060, "CS", b"CT".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x0008);
    assert_eq!(tag.element, 0x0060);
    assert_eq!(tag.vr, "CS");
    assert_eq!(tag.value, b"CT");
}

#[test]
fn parse_dicom_with_multiple_tags() {
    let tags = vec![
        (0x0010, 0x0010, "PN", b"Doe^Jane".to_vec()),
        (0x0010, 0x0020, "LO", b"P12345".to_vec()),
        (0x0008, 0x0020, "DA", b"20250205".to_vec()),
        (0x0008, 0x0060, "CS", b"CT".to_vec()),
    ];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 4);

    assert_eq!(obj.tags[0].vr, "PN");
    assert_eq!(obj.tags[1].vr, "LO");
    assert_eq!(obj.tags[2].vr, "DA");
    assert_eq!(obj.tags[3].vr, "CS");
}

#[test]
fn parse_dicom_with_binary_tag() {
    let binary_data = vec![0x00, 0x01, 0x02, 0x03, 0x04, 0x05];
    let tags = vec![(0x7FE0, 0x0010, "OB", binary_data.clone())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x7FE0);
    assert_eq!(tag.element, 0x0010);
    assert_eq!(tag.vr, "OB");
    assert_eq!(tag.value_length, 6);
    assert_eq!(tag.value, binary_data);
}

#[test]
fn parse_dicom_with_ui_tag() {
    let uid = b"1.2.840.10008.5.1.4.1.1.2".to_vec();
    let tags = vec![(0x0008, 0x0016, "UI", uid.clone())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.vr, "UI");
    assert_eq!(tag.value, uid);
}

#[test]
fn parse_dicom_with_long_value() {
    let long_value = vec![b'A'; 1000];
    let tags = vec![(0x0010, 0x0010, "LO", long_value.clone())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.value_length, 1000);
    assert_eq!(tag.value, long_value);
}

#[test]
fn parse_dicom_empty_value() {
    let tags = vec![(0x0010, 0x0010, "PN", vec![])];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.value_length, 0);
    assert_eq!(tag.value.len(), 0);
}

#[test]
fn parse_dicom_with_time_tag() {
    let tags = vec![(0x0008, 0x0030, "TM", b"120000".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.vr, "TM");
    assert_eq!(tag.value, b"120000");
}

#[test]
fn parse_dicom_with_integer_tag() {
    let tags = vec![(0x0028, 0x0010, "US", b"512".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.vr, "US");
    assert_eq!(tag.value, b"512");
}

#[test]
fn parse_dicom_missing_magic() {
    let mut bytes = vec![0u8; 132];
    bytes.extend_from_slice(b"XXXX");

    let result = parse_dicom(&bytes);
    assert!(result.is_err());
}

#[test]
fn parse_dicom_file_too_small() {
    let bytes = vec![0u8; 100];
    let result = parse_dicom(&bytes);
    assert!(result.is_err());
}

#[test]
fn parse_dicom_with_series_number_tag() {
    let tags = vec![(0x0020, 0x0011, "IS", b"1".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.group, 0x0020);
    assert_eq!(tag.element, 0x0011);
    assert_eq!(tag.vr, "IS");
}

#[test]
fn parse_dicom_with_instance_number_tag() {
    let tags = vec![(0x0020, 0x0013, "IS", b"42".to_vec())];
    let bytes = create_dicom_with_tags(tags);

    let obj = parse_dicom(&bytes).expect("parse");
    assert_eq!(obj.tags.len(), 1);

    let tag = &obj.tags[0];
    assert_eq!(tag.vr, "IS");
    assert_eq!(tag.value, b"42");
}
