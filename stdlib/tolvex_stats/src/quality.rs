pub fn hedis_rate(numerator: f64, denominator: f64) -> f64 {
    if denominator == 0.0 {
        f64::NAN
    } else {
        numerator / denominator
    }
}

pub fn star_rating(score: f64, thresholds: &[f64; 4]) -> u8 {
    // thresholds ascending for 2,3,4,5 star cutpoints
    if score < thresholds[0] {
        1
    } else if score < thresholds[1] {
        2
    } else if score < thresholds[2] {
        3
    } else if score < thresholds[3] {
        4
    } else {
        5
    }
}
