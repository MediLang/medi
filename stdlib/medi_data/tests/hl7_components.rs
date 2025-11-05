use medi_data::hl7::parse_field_components;

#[test]
fn parse_components_subcomponents_repetitions() {
    let field = "A^B&C^D~E^F&";
    let parsed = parse_field_components(field);
    // Two repetitions: ["A^B&C^D", "E^F&"]
    assert_eq!(parsed.len(), 2);
    // First repetition, components: A | B&C | D
    assert_eq!(parsed[0].len(), 3);
    assert_eq!(parsed[0][0], vec!["A".to_string()]);
    assert_eq!(parsed[0][1], vec!["B".to_string(), "C".to_string()]);
    assert_eq!(parsed[0][2], vec!["D".to_string()]);
    // Second repetition, components: E | F& (empty subcomponent becomes empty string last)
    assert_eq!(parsed[1].len(), 2);
    assert_eq!(parsed[1][0], vec!["E".to_string()]);
    assert_eq!(parsed[1][1], vec!["F".to_string(), "".to_string()]);
}
