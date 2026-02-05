#[derive(Debug, Clone, PartialEq)]
pub struct DicomObject {
    pub has_preamble: bool,
    pub magic_ok: bool,
    pub tags: Vec<DicomTag>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DicomTag {
    pub group: u16,
    pub element: u16,
    pub vr: String,
    pub value_length: u32,
    pub value: Vec<u8>,
}

pub fn parse_dicom(bytes: &[u8]) -> Result<DicomObject, DicomError> {
    if bytes.len() < 132 {
        return Err(DicomError::new("DICOM file too small"));
    }

    let magic_ok = &bytes[128..132] == b"DICM";
    if !magic_ok {
        return Err(DicomError::new("Missing DICM magic"));
    }

    if bytes.len() < 140 {
        return Err(DicomError::new("DICOM truncated after preamble"));
    }

    let mut tags = Vec::new();
    let mut pos = 132;

    while pos + 8 <= bytes.len() {
        let group = u16::from_le_bytes([bytes[pos], bytes[pos + 1]]);
        let element = u16::from_le_bytes([bytes[pos + 2], bytes[pos + 3]]);

        if group == 0 && element == 0 && pos == 132 {
            return Err(DicomError::new("Invalid first tag (0x0000,0x0000)"));
        }

        let vr0 = bytes[pos + 4];
        let vr1 = bytes[pos + 5];

        if !(vr0.is_ascii_uppercase() && vr1.is_ascii_uppercase()) {
            break;
        }

        let vr = format!("{}{}", vr0 as char, vr1 as char);

        let (value_length, tag_header_len) = parse_vr_length(bytes, pos + 6, &vr)?;

        let value_start = pos + 6 + tag_header_len;
        let value_end = value_start + value_length as usize;

        if value_end > bytes.len() {
            break;
        }

        let value = bytes[value_start..value_end].to_vec();

        tags.push(DicomTag {
            group,
            element,
            vr,
            value_length,
            value,
        });

        pos = value_end;
    }

    Ok(DicomObject {
        has_preamble: true,
        magic_ok,
        tags,
    })
}

fn parse_vr_length(bytes: &[u8], pos: usize, vr: &str) -> Result<(u32, usize), DicomError> {
    if pos + 2 > bytes.len() {
        return Err(DicomError::new("VR length field truncated"));
    }

    match vr {
        "OB" | "OD" | "OF" | "OL" | "OW" | "UN" | "UC" | "UR" | "UT" => {
            if pos + 6 > bytes.len() {
                return Err(DicomError::new("Explicit VR with reserved bytes truncated"));
            }
            let length = u32::from_le_bytes([
                bytes[pos + 2],
                bytes[pos + 3],
                bytes[pos + 4],
                bytes[pos + 5],
            ]);
            Ok((length, 6))
        }
        _ => {
            let length = u16::from_le_bytes([bytes[pos], bytes[pos + 1]]) as u32;
            Ok((length, 2))
        }
    }
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
