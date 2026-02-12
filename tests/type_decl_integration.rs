use tlvxc::parser::parse_program;
use tlvxc::type_checker::DefaultTypeChecker;
use tlvxc::types::MediType;

#[test]
fn type_decl_end_to_end_person() {
    let code = r#"
        type Person { name: String, age: Int };
        let p: Person;
        p.name = "Alice";
    "#;

    let (_rest, stmts) = parse_program(code).expect("program should parse");

    // Run type checker over the parsed statements
    let mut checker = DefaultTypeChecker::new();
    for s in &stmts {
        if let Err(e) = checker.check_stmt(s) {
            panic!("type error: {e}");
        }
    }

    // Verify that `p` is bound to a Struct(Person) with the expected fields
    let env = checker.env();
    let p_ty = env.get("p").cloned().expect("p should be bound");
    match p_ty {
        MediType::Struct(map) => {
            assert_eq!(map.get("name"), Some(&MediType::String));
            assert_eq!(map.get("age"), Some(&MediType::Int));
        }
        other => panic!("expected p to be Struct(Person), got {other:?}"),
    }
}
