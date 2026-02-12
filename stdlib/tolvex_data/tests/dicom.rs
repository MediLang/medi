use tolvex_data::dicom::parse_dicom;

#[test]
fn dicom_magic_ok() {
    let mut bytes = vec![0u8; 140];
    bytes[128..132].copy_from_slice(b"DICM");
    // First tag group/element (non-zero): (0x0008,0x0005)
    bytes[132] = 0x08;
    bytes[133] = 0x00; // group LE
    bytes[134] = 0x05;
    bytes[135] = 0x00; // element LE
                       // VR bytes: 'L','O'
    bytes[136] = b'L';
    bytes[137] = b'O';
    let obj = parse_dicom(&bytes).expect("parse ok");
    assert!(obj.has_preamble);
}

#[test]
fn dicom_magic_missing() {
    let bytes = vec![0u8; 132];
    let err = parse_dicom(&bytes).unwrap_err();
    assert!(err.message.contains("DICM"));
}
