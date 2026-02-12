#[derive(Debug, Clone, PartialEq)]
pub struct HL7Message {
    pub segments: Vec<HL7Segment>,
    pub field_sep: char,
    pub component_sep: char,
    pub repetition_sep: char,
    pub escape_char: char,
    pub subcomponent_sep: char,
}

#[derive(Debug, Clone, PartialEq)]
pub struct HL7Segment {
    pub name: String,
    pub fields: Vec<String>,
}

pub fn parse_hl7(input: &str) -> Result<HL7Message, HL7Error> {
    if input.trim().is_empty() {
        return Ok(HL7Message {
            segments: vec![],
            field_sep: '|',
            component_sep: '^',
            repetition_sep: '~',
            escape_char: '\\',
            subcomponent_sep: '&',
        });
    }

    let lines: Vec<&str> = input.split('\r').collect();
    if lines.is_empty() {
        return Ok(HL7Message {
            segments: vec![],
            field_sep: '|',
            component_sep: '^',
            repetition_sep: '~',
            escape_char: '\\',
            subcomponent_sep: '&',
        });
    }

    let first_line = lines[0];
    if !first_line.starts_with("MSH") {
        return Err(HL7Error::new("HL7 message must start with MSH segment"));
    }

    let (field_sep, component_sep, repetition_sep, escape_char, subcomponent_sep) =
        extract_separators(first_line)?;

    let mut segments = Vec::new();

    for line in lines {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let seg_name = if line.len() >= 3 {
            &line[0..3]
        } else {
            return Err(HL7Error::new(format!("invalid segment name: {line}")));
        };

        if seg_name.len() != 3
            || !seg_name
                .chars()
                .all(|c| c.is_ascii_uppercase() || c.is_ascii_digit())
        {
            return Err(HL7Error::new(format!("invalid segment name: {seg_name}")));
        }

        let fields = if seg_name == "MSH" {
            parse_msh_fields(line, field_sep)?
        } else {
            parse_segment_fields(line, field_sep)?
        };

        segments.push(HL7Segment {
            name: seg_name.to_string(),
            fields,
        });
    }

    Ok(HL7Message {
        segments,
        field_sep,
        component_sep,
        repetition_sep,
        escape_char,
        subcomponent_sep,
    })
}

fn extract_separators(msh_line: &str) -> Result<(char, char, char, char, char), HL7Error> {
    if msh_line.len() < 9 {
        return Err(HL7Error::new("MSH segment too short to extract separators"));
    }

    let field_sep = msh_line.chars().nth(3).unwrap_or('|');
    let encoding = &msh_line[4..8];
    if encoding.len() != 4 {
        return Err(HL7Error::new("MSH encoding characters must be 4 chars"));
    }

    let component_sep = encoding.chars().next().unwrap_or('^');
    let repetition_sep = encoding.chars().nth(1).unwrap_or('~');
    let escape_char = encoding.chars().nth(2).unwrap_or('\\');
    let subcomponent_sep = encoding.chars().nth(3).unwrap_or('&');

    Ok((
        field_sep,
        component_sep,
        repetition_sep,
        escape_char,
        subcomponent_sep,
    ))
}

fn parse_msh_fields(line: &str, field_sep: char) -> Result<Vec<String>, HL7Error> {
    if line.len() < 4 {
        return Err(HL7Error::new("MSH segment too short"));
    }

    let mut fields = Vec::new();
    fields.push(line[3..4].to_string());

    let rest = &line[4..];
    let field_parts: Vec<&str> = rest.split(field_sep).collect();
    for part in field_parts {
        fields.push(part.to_string());
    }

    Ok(fields)
}

fn parse_segment_fields(line: &str, field_sep: char) -> Result<Vec<String>, HL7Error> {
    if line.len() < 4 {
        return Err(HL7Error::new("segment too short"));
    }

    let rest = &line[4..];
    let field_parts: Vec<&str> = rest.split(field_sep).collect();
    let mut fields = Vec::new();

    for part in field_parts {
        fields.push(part.to_string());
    }

    Ok(fields)
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
