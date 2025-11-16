pub fn moving_average(xs: &[f64], window: usize) -> Vec<f64> {
    if window == 0 || xs.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::with_capacity(xs.len().saturating_sub(window) + 1);
    let mut sum = 0.0;
    for i in 0..xs.len() {
        sum += xs[i];
        if i >= window {
            sum -= xs[i - window];
        }
        if i + 1 >= window {
            out.push(sum / (window as f64));
        }
    }
    out
}

pub fn exponential_moving_average(xs: &[f64], alpha: f64) -> Vec<f64> {
    if xs.is_empty() {
        return Vec::new();
    }
    let mut out = Vec::with_capacity(xs.len());
    let mut ema = xs[0];
    out.push(ema);
    for &x in &xs[1..] {
        ema = alpha * x + (1.0 - alpha) * ema;
        out.push(ema);
    }
    out
}

pub fn rolling_std(xs: &[f64], window: usize) -> Vec<f64> {
    if window == 0 || xs.len() < window {
        return Vec::new();
    }
    let mut out = Vec::with_capacity(xs.len() - window + 1);
    let mut sum = 0.0;
    let mut sumsq = 0.0;
    for i in 0..xs.len() {
        let x = xs[i];
        sum += x;
        sumsq += x * x;
        if i >= window {
            let y = xs[i - window];
            sum -= y;
            sumsq -= y * y;
        }
        if i + 1 >= window {
            let n = window as f64;
            let mean = sum / n;
            let var = (sumsq - n * mean * mean) / (n - 1.0);
            out.push(var.max(0.0).sqrt());
        }
    }
    out
}
