pub fn bin_series(xs: &[f64], bin_edges: &[f64]) -> Vec<usize> {
    if bin_edges.len() < 2 {
        return vec![];
    }
    let mut counts = vec![0usize; bin_edges.len() - 1];
    for &x in xs {
        for i in 0..bin_edges.len() - 1 {
            if x >= bin_edges[i] && x < bin_edges[i + 1] {
                counts[i] += 1;
                break;
            }
            if i == bin_edges.len() - 2 && x == bin_edges[i + 1] {
                counts[i] += 1;
            }
        }
    }
    counts
}

pub fn normalize_min_max(xs: &[f64]) -> Vec<f64> {
    if xs.is_empty() {
        return vec![];
    }
    let min = xs.iter().cloned().fold(f64::INFINITY, f64::min);
    let max = xs.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let range = max - min;
    if range == 0.0 {
        return vec![0.0; xs.len()];
    }
    xs.iter().map(|&x| (x - min) / range).collect()
}

pub fn sparkline_ascii(xs: &[f64], width: usize) -> String {
    if xs.is_empty() || width == 0 {
        return String::new();
    }

    let mut sampled = Vec::with_capacity(width);
    if width == 1 {
        sampled.push(xs[xs.len() - 1]);
    } else {
        for i in 0..width {
            let idx = (i * (xs.len() - 1)) / (width - 1);
            sampled.push(xs[idx]);
        }
    }

    let norm = normalize_min_max(&sampled);
    let levels = b" .:-=+*#%@";
    let mut out = String::with_capacity(width);
    for &z in &norm {
        let mut i = (z * ((levels.len() - 1) as f64)).round() as usize;
        if i >= levels.len() {
            i = levels.len() - 1;
        }
        out.push(levels[i] as char);
    }
    out
}

pub fn histogram_ascii(xs: &[f64], bin_edges: &[f64], max_width: usize) -> Vec<String> {
    if max_width == 0 {
        return Vec::new();
    }

    let counts = bin_series(xs, bin_edges);
    if counts.is_empty() {
        return Vec::new();
    }

    let max_count = *counts.iter().max().unwrap_or(&0);
    let mut lines = Vec::with_capacity(counts.len());
    for (i, &c) in counts.iter().enumerate() {
        let bar_len = if max_count == 0 {
            0
        } else {
            (c * max_width) / max_count
        };
        let mut line = String::new();
        if i + 1 < bin_edges.len() {
            line.push_str(&format!("[{:.3},{:.3}): ", bin_edges[i], bin_edges[i + 1]));
        } else {
            line.push_str("[?,?): ");
        }
        line.push_str(&"#".repeat(bar_len));
        line.push_str(&format!(" ({c})"));
        lines.push(line);
    }
    lines
}

pub fn line_chart_svg(xs: &[f64], width: u32, height: u32) -> String {
    if xs.is_empty() || width == 0 || height == 0 {
        return String::new();
    }

    let norm = normalize_min_max(xs);
    let w = width as f64;
    let h = height as f64;

    let mut points = String::new();
    if xs.len() == 1 {
        let y = h - norm[0] * h;
        points.push_str(&format!("0,{y:.3}"));
    } else {
        for (i, &z) in norm.iter().enumerate() {
            let x = (i as f64) * (w / ((xs.len() - 1) as f64));
            let y = h - z * h;
            if i > 0 {
                points.push(' ');
            }
            points.push_str(&format!("{x:.3},{y:.3}"));
        }
    }

    format!(
        "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"{width}\" height=\"{height}\"><polyline fill=\"none\" stroke=\"black\" stroke-width=\"1\" points=\"{points}\" /></svg>"
    )
}
