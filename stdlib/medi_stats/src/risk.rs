pub fn odds(p: f64) -> f64 {
    if p >= 1.0 {
        f64::INFINITY
    } else if p <= 0.0 {
        0.0
    } else {
        p / (1.0 - p)
    }
}

pub fn logit(p: f64) -> f64 {
    let o = odds(p);
    if o == 0.0 {
        f64::NEG_INFINITY
    } else if !o.is_finite() {
        f64::INFINITY
    } else {
        o.ln()
    }
}

pub fn inv_logit(x: f64) -> f64 {
    1.0 / (1.0 + (-x).exp())
}

pub fn risk_ratio(p_treat: f64, p_ctrl: f64) -> f64 {
    if p_ctrl == 0.0 {
        f64::NAN
    } else {
        p_treat / p_ctrl
    }
}

pub fn risk_difference(p_treat: f64, p_ctrl: f64) -> f64 {
    p_treat - p_ctrl
}
