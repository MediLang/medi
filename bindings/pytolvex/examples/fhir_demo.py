import pymedi

patient = {
    "id": "p1",
    "given_name": "Jane",
    "family_name": "Doe",
    "birth_date": "1985-12-24",
}

print(pymedi.validate_fhir_patient(patient))
print(pymedi.fhir_query_stub("Patient", {"family_name": "Doe"}))
