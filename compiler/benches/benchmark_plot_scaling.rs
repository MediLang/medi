use plotters::prelude::*;
use plotters_svg::SVGBackend;
use std::fs;
use std::path::PathBuf;

#[derive(Debug)]
#[allow(dead_code)]
struct Row {
    kind: String,
    size: String,
    bytes: u64,
    ok: bool,
    stmts: usize,
    time_ms: f64,
}

fn parse_csv(p: &str) -> std::io::Result<Vec<Row>> {
    let s = fs::read_to_string(p)?;
    let mut rows = Vec::new();
    for (i, line) in s.lines().enumerate() {
        if i == 0 {
            continue;
        }
        if line.trim().is_empty() {
            continue;
        }
        let parts: Vec<&str> = line.split(',').collect();
        if parts.len() < 6 {
            continue;
        }
        rows.push(Row {
            kind: parts[0].to_string(),
            size: parts[1].to_string(),
            bytes: parts[2].parse().unwrap_or(0),
            ok: parts[3].trim() == "true",
            stmts: parts[4].parse().unwrap_or(0),
            time_ms: parts[5].parse().unwrap_or(0.0),
        });
    }
    Ok(rows)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("benchdata");
    // Pick latest CSV
    let mut entries: Vec<_> = fs::read_dir(&base)?
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map(|x| x == "csv").unwrap_or(false))
        .collect();
    entries.sort();
    let csv = entries
        .last()
        .ok_or("No CSV found; run benchmark_parser_scaling first")?
        .clone();
    let data = parse_csv(csv.to_str().unwrap())?;

    let kinds = {
        let mut ks: Vec<String> = data.iter().map(|r| r.kind.clone()).collect();
        ks.sort();
        ks.dedup();
        ks
    };

    // Linear plot
    let out_svg = base.join("scaling_plot.svg");
    {
        let root = SVGBackend::new(out_svg.to_str().unwrap(), (900, 540)).into_drawing_area();
        root.fill(&WHITE)?;
        let x_max = data.iter().map(|r| r.bytes).max().unwrap_or(1);
        let y_max = data
            .iter()
            .map(|r| r.time_ms)
            .fold(f64::NAN, f64::max)
            .max(1.0);
        let mut chart = ChartBuilder::on(&root)
            .caption(
                "Parser scaling (avg over 3 runs)",
                ("sans-serif", 20).into_font(),
            )
            .margin(10)
            .x_label_area_size(45)
            .y_label_area_size(60)
            .build_cartesian_2d(0u64..x_max, 0f64..y_max)?;
        chart
            .configure_mesh()
            .x_desc("File size (bytes)")
            .y_desc("Time (ms)")
            .draw()?;
        for (i, kind) in kinds.iter().enumerate() {
            let mut pts: Vec<(u64, f64)> = data
                .iter()
                .filter(|r| &r.kind == kind)
                .map(|r| (r.bytes, r.time_ms))
                .collect();
            pts.sort_by_key(|p| p.0);
            let color = Palette99::pick(i).mix(1.0);
            chart
                .draw_series(LineSeries::new(pts.clone(), color))?
                .label(kind.as_str())
                .legend(move |(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], color));
            chart.draw_series(pts.into_iter().map(|p| Circle::new(p, 3, color.filled())))?;
        }
        chart.configure_series_labels().border_style(BLACK).draw()?;
        root.present()?;
    }

    // Log-Log plot
    let out_svg_log = base.join("scaling_plot_loglog.svg");
    {
        let root = SVGBackend::new(out_svg_log.to_str().unwrap(), (900, 540)).into_drawing_area();
        root.fill(&WHITE)?;
        let x_max = data.iter().map(|r| r.bytes).max().unwrap_or(1);
        let y_max = data
            .iter()
            .map(|r| r.time_ms)
            .fold(f64::NAN, f64::max)
            .max(1.0);
        // Use transformed axes by plotting points with log transform
        let mut chart = ChartBuilder::on(&root)
            .caption("Parser scaling (log-log)", ("sans-serif", 20).into_font())
            .margin(10)
            .x_label_area_size(45)
            .y_label_area_size(60)
            .build_cartesian_2d(0f64..(x_max as f64).ln_1p(), 0f64..y_max.ln_1p())?;
        chart
            .configure_mesh()
            .x_desc("ln(1+bytes)")
            .y_desc("ln(1+time_ms)")
            .draw()?;
        for (i, kind) in kinds.iter().enumerate() {
            let mut pts: Vec<(f64, f64)> = data
                .iter()
                .filter(|r| &r.kind == kind)
                .map(|r| ((r.bytes as f64).ln_1p(), (r.time_ms).ln_1p()))
                .collect();
            pts.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
            let color = Palette99::pick(i).mix(1.0);
            chart
                .draw_series(LineSeries::new(pts.clone(), color))?
                .label(kind.as_str())
                .legend(move |(x, y)| PathElement::new(vec![(x, y), (x + 20, y)], color));
            chart.draw_series(pts.into_iter().map(|p| Circle::new(p, 3, color.filled())))?;
        }
        chart.configure_series_labels().border_style(BLACK).draw()?;
        root.present()?;
    }

    eprintln!("Wrote {} and {}", out_svg.display(), out_svg_log.display());
    Ok(())
}
