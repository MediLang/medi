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
