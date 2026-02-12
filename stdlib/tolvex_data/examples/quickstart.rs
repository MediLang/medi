use tolvex_data::hl7_fhir::hl7_to_fhir_patient_minimal;
use tolvex_data::validate::validate_patient;

fn main() {
    let hl7 = "MSH|^~\\&|SND|SRC|RCV|DST|20250101||ADT^A01|MSGID|P|2.3\rPID|||12345||DOE^JOHN||19600101|M\r";
    let patient = hl7_to_fhir_patient_minimal(hl7).expect("hl7->fhir");
    validate_patient(&patient).expect("validate");
    println!("patient id={}", patient.id);
}
