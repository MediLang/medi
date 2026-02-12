use core::cmp::Ordering;
use rand::Rng;

pub fn laplace_scale(sensitivity: f64, epsilon: f64) -> Option<f64> {
    if sensitivity < 0.0 {
        return None;
    }
    if epsilon.partial_cmp(&0.0) != Some(Ordering::Greater) {
        return None;
    }
    Some(sensitivity / epsilon)
}

pub fn laplace_noise(scale: f64, rng: &mut impl Rng) -> Option<f64> {
    if scale.partial_cmp(&0.0) != Some(Ordering::Greater) {
        return None;
    }

    let u: f64 = rng.gen_range(-0.5..0.5);
    let s = if u >= 0.0 { 1.0 } else { -1.0 };
    let mag = (1.0 - (2.0 * u.abs())).ln();
    Some(-scale * s * mag)
}

pub fn laplace_mechanism(
    value: f64,
    sensitivity: f64,
    epsilon: f64,
    rng: &mut impl Rng,
) -> Option<f64> {
    let scale = laplace_scale(sensitivity, epsilon)?;
    let n = laplace_noise(scale, rng)?;
    Some(value + n)
}

pub fn gaussian_sigma(sensitivity: f64, epsilon: f64, delta: f64) -> Option<f64> {
    if sensitivity < 0.0 {
        return None;
    }
    if epsilon.partial_cmp(&0.0) != Some(Ordering::Greater) {
        return None;
    }
    if delta.partial_cmp(&0.0) != Some(Ordering::Greater) || delta >= 1.0 {
        return None;
    }

    let c = (2.0 * (1.25 / delta).ln()).sqrt();
    Some(c * sensitivity / epsilon)
}

pub fn gaussian_noise(sigma: f64, rng: &mut impl Rng) -> Option<f64> {
    if sigma.partial_cmp(&0.0) != Some(Ordering::Greater) {
        return None;
    }

    let u1: f64 = rng.gen_range(0.0..1.0);
    let u2: f64 = rng.gen_range(0.0..1.0);
    let r = (-2.0 * u1.ln()).sqrt();
    let theta = 2.0 * core::f64::consts::PI * u2;
    let z0 = r * theta.cos();
    Some(z0 * sigma)
}

pub fn gaussian_mechanism(
    value: f64,
    sensitivity: f64,
    epsilon: f64,
    delta: f64,
    rng: &mut impl Rng,
) -> Option<f64> {
    let sigma = gaussian_sigma(sensitivity, epsilon, delta)?;
    let n = gaussian_noise(sigma, rng)?;
    Some(value + n)
}
