#[derive(Debug, Clone, PartialEq)]
pub struct DicomObject {
    pub has_preamble: bool,
    pub magic_ok: bool,
}

pub fn parse_dicom(bytes: &[u8]) -> Result<DicomObject, DicomError> {
    if bytes.len() < 132 {
        return Err(DicomError::new("DICOM file too small"));
    }
    // First 128 bytes are preamble (any value, typically 0)
    // Next 4 bytes should be ASCII 'D','I','C','M'
    let magic_ok = &bytes[128..132] == b"DICM";
    if !magic_ok {
        return Err(DicomError::new("Missing DICM magic"));
    }
    // Very light plausibility check: ensure there are at least 8 bytes for a tag header afterwards
    if bytes.len() < 140 {
        return Err(DicomError::new("DICOM truncated after preamble"));
    }
    // Read first tag group/element (little-endian)
    let group = u16::from_le_bytes([bytes[132], bytes[133]]);
    let element = u16::from_le_bytes([bytes[134], bytes[135]]);
    if group == 0 && element == 0 {
        return Err(DicomError::new("Invalid first tag (0x0000,0x0000)"));
    }
    // Naive explicit VR plausibility: next two bytes look like uppercase letters (e.g., PN, LO, UI)
    let vr0 = bytes[136];
    let vr1 = bytes[137];
    if !(vr0.is_ascii_uppercase() && vr1.is_ascii_uppercase()) {
        return Err(DicomError::new(
            "Invalid or unexpected VR bytes after first tag",
        ));
    }
    Ok(DicomObject {
        has_preamble: true,
        magic_ok,
    })
}

#[derive(Debug, Clone, PartialEq)]
pub struct DicomError {
    pub message: String,
}

impl DicomError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}
