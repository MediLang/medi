use medi_stats::{histogram_ascii, line_chart_svg, sparkline_ascii};

#[test]
fn sparkline_ascii_basic() {
    let xs = [0.0, 1.0, 2.0, 3.0];
    let s = sparkline_ascii(&xs, 4);
    assert_eq!(s.len(), 4);
    assert!(!s.is_empty());

    let empty = sparkline_ascii(&[], 10);
    assert!(empty.is_empty());

    let zero_width = sparkline_ascii(&xs, 0);
    assert!(zero_width.is_empty());
}

#[test]
fn histogram_ascii_basic() {
    let xs = [0.0, 0.1, 0.9, 1.0, 1.5, 2.0];
    let edges = [0.0, 1.0, 2.0];
    let lines = histogram_ascii(&xs, &edges, 10);
    assert_eq!(lines.len(), 2);

    // Should include counts and some bars
    assert!(lines[0].contains("(3)"));
    assert!(lines[1].contains("(3)"));

    let empty = histogram_ascii(&xs, &[0.0], 10);
    assert!(empty.is_empty());

    let zero_width = histogram_ascii(&xs, &edges, 0);
    assert!(zero_width.is_empty());
}

#[test]
fn line_chart_svg_basic() {
    let xs = [1.0, 2.0, 3.0];
    let svg = line_chart_svg(&xs, 100, 50);
    assert!(svg.starts_with("<svg"));
    assert!(svg.contains("polyline"));
    assert!(svg.contains("points=\""));

    let empty = line_chart_svg(&[], 100, 50);
    assert!(empty.is_empty());

    let empty2 = line_chart_svg(&xs, 0, 50);
    assert!(empty2.is_empty());

    let single = line_chart_svg(&[1.0], 10, 10);
    assert!(single.contains("points=\"0,"));
}
