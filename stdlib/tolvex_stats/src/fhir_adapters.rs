#![cfg(feature = "fhir")]
use tolvex_data::fhir::FHIRObservation;

pub fn extract_series_by_code<'a>(
    obs: impl IntoIterator<Item = &'a FHIRObservation>,
    code: &str,
) -> Vec<f64> {
    let mut out = Vec::new();
    for o in obs {
        if o.code == code {
            if let Some(v) = o.value {
                if v.is_finite() {
                    out.push(v);
                }
            }
        }
    }
    out
}
