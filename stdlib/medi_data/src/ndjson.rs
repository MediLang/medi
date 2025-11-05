use serde::{de::DeserializeOwned, Serialize};

/// Write newline-delimited JSON items to a string buffer
pub fn to_ndjson<T: Serialize>(items: &[T]) -> Result<String, serde_json::Error> {
    let mut out = String::new();
    for item in items {
        let line = serde_json::to_string(item)?;
        out.push_str(&line);
        out.push('\n');
    }
    Ok(out)
}

/// Parse newline-delimited JSON items from &str
pub fn from_ndjson<T: DeserializeOwned>(s: &str) -> Result<Vec<T>, serde_json::Error> {
    let mut v = Vec::new();
    for line in s.lines() {
        if line.trim().is_empty() {
            continue;
        }
        v.push(serde_json::from_str::<T>(line)?);
    }
    Ok(v)
}
