#[derive(Debug, Clone, PartialEq)]
pub struct DeviceReading {
    pub device_id: String,
    pub metric: String,
    pub value: f64,
    pub timestamp_ms: u64,
}

impl DeviceReading {
    pub fn new(
        device_id: impl Into<String>,
        metric: impl Into<String>,
        value: f64,
        timestamp_ms: u64,
    ) -> Self {
        Self {
            device_id: device_id.into(),
            metric: metric.into(),
            value,
            timestamp_ms,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeviceProtocol {
    Hl7,
    Dicom,
    Proprietary(String),
}

pub fn parse_device_message(protocol: DeviceProtocol, msg: &str) -> Option<DeviceReading> {
    match protocol {
        // Minimal placeholder parsing: "device_id,metric,value,timestamp_ms"
        DeviceProtocol::Hl7 | DeviceProtocol::Dicom | DeviceProtocol::Proprietary(_) => {
            let parts: Vec<&str> = msg.split(',').collect();
            if parts.len() != 4 {
                return None;
            }
            let value: f64 = parts[2].parse().ok()?;
            let ts: u64 = parts[3].parse().ok()?;
            Some(DeviceReading::new(parts[0], parts[1], value, ts))
        }
    }
}
