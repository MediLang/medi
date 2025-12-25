use medi_data::fhir::FHIRPatient;
use medi_data::validate::validate_patient;
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::PyDict;

#[pyfunction]
fn mean(xs: Vec<f64>) -> PyResult<f64> {
    Ok(medi_stats::mean(&xs))
}

#[pyfunction]
fn validate_fhir_patient(patient: &Bound<'_, PyDict>) -> PyResult<bool> {
    let id: String = patient
        .get_item("id")?
        .ok_or_else(|| PyValueError::new_err("patient['id'] is required"))?
        .extract()?;

    let given_name: Option<String> = match patient.get_item("given_name")? {
        Some(v) => v.extract()?,
        None => None,
    };

    let family_name: Option<String> = match patient.get_item("family_name")? {
        Some(v) => v.extract()?,
        None => None,
    };

    let birth_date: Option<String> = match patient.get_item("birth_date")? {
        Some(v) => v.extract()?,
        None => None,
    };

    let p = FHIRPatient {
        id,
        given_name,
        family_name,
        birth_date,
    };

    validate_patient(&p).map_err(|e| PyValueError::new_err(e.to_string()))?;
    Ok(true)
}

#[pyfunction]
fn fhir_query_stub(resource_type: String, query: &Bound<'_, PyDict>) -> PyResult<String> {
    let keys: Vec<String> = query
        .keys()
        .iter()
        .filter_map(|k| k.extract::<String>().ok())
        .collect();
    Ok(format!(
        "fhir_query_stub(resource_type={resource_type}, keys={})",
        keys.join(",")
    ))
}

#[pymodule]
fn pymedi(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(mean, m)?)?;
    m.add_function(wrap_pyfunction!(validate_fhir_patient, m)?)?;
    m.add_function(wrap_pyfunction!(fhir_query_stub, m)?)?;
    Ok(())
}
