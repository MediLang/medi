#[derive(Debug, Clone, PartialEq)]
pub struct HL7Message {
    pub segments: Vec<HL7Segment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HL7Segment {
    pub name: String,
    pub fields: Vec<String>,
}

pub fn parse_hl7(input: &str) -> Result<HL7Message, HL7Error> {
    if input.trim().is_empty() {
        return Ok(HL7Message { segments: vec![] });
    }
    let mut segments = Vec::new();
    for line in input.split(['\r', '\n']) {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        let mut parts = line.split('|');
        let name = parts.next().unwrap_or("").trim().to_string();
        if name.len() != 3 {
            return Err(HL7Error::new(format!("invalid segment name: {name}")));
        }
        let fields = parts.map(|s| s.trim().to_string()).collect();
        segments.push(HL7Segment { name, fields });
    }
    Ok(HL7Message { segments })
}

#[derive(Debug, Clone, PartialEq)]
pub struct HL7Error {
    pub message: String,
}

impl HL7Error {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

/// Parse one HL7 field into repetitions/components/subcomponents using separators ~, ^, &.
/// Returns a 3-level nested vector: [repetition][component][subcomponent]
pub fn parse_field_components(field: &str) -> Vec<Vec<Vec<String>>> {
    field
        .split('~')
        .map(|rep| {
            rep.split('^')
                .map(|comp| {
                    comp.split('&')
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                })
                .collect::<Vec<Vec<String>>>()
        })
        .collect::<Vec<Vec<Vec<String>>>>()
}
