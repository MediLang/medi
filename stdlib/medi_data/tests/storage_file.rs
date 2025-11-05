use medi_data::fhir::FHIRPatient;
use medi_data::storage::SecureStore;
use medi_data::storage_file::FileStore;

#[test]
fn file_store_list_and_remove() {
    let tmp = tempfile::tempdir().expect("tmpdir");
    let store = FileStore::new(tmp.path()).expect("store");

    let p1 = FHIRPatient {
        id: "p1".into(),
        given_name: None,
        family_name: None,
        birth_date: None,
    };
    let p2 = FHIRPatient {
        id: "p2".into(),
        given_name: None,
        family_name: None,
        birth_date: None,
    };

    store.save("patient_p1", &p1).expect("save p1");
    store.save("patient_p2", &p2).expect("save p2");

    let mut keys = store.list_keys().expect("list");
    keys.sort();
    assert_eq!(keys, vec!["patient_p1", "patient_p2"]);

    store.remove("patient_p1").expect("remove p1");
    let mut keys2 = store.list_keys().expect("list2");
    keys2.sort();
    assert_eq!(keys2, vec!["patient_p2"]);

    let loaded: Option<FHIRPatient> = store.load("patient_p1").expect("load");
    assert!(loaded.is_none());
}
