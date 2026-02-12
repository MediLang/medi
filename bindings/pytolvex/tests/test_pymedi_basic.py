import pymedi


def test_mean():
    assert pymedi.mean([1.0, 2.0, 3.0]) == 2.0


def test_validate_patient_ok():
    assert (
        pymedi.validate_fhir_patient(
            {
                "id": "p1",
                "given_name": "Jane",
                "family_name": "Doe",
                "birth_date": "1985-12-24",
            }
        )
        is True
    )


def test_validate_patient_error():
    try:
        pymedi.validate_fhir_patient({"id": ""})
        assert False
    except ValueError:
        assert True
