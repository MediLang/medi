pub fn incidence_rate(new_cases: f64, person_time: f64) -> f64 {
    if person_time == 0.0 {
        f64::NAN
    } else {
        new_cases / person_time
    }
}

pub fn prevalence(existing_cases: f64, population: f64) -> f64 {
    if population == 0.0 {
        f64::NAN
    } else {
        existing_cases / population
    }
}

pub fn sensitivity(tp: f64, fn_: f64) -> f64 {
    let denom = tp + fn_;
    if denom == 0.0 {
        f64::NAN
    } else {
        tp / denom
    }
}

pub fn specificity(tn: f64, fp: f64) -> f64 {
    let denom = tn + fp;
    if denom == 0.0 {
        f64::NAN
    } else {
        tn / denom
    }
}

pub fn ppv(tp: f64, fp: f64) -> f64 {
    let denom = tp + fp;
    if denom == 0.0 {
        f64::NAN
    } else {
        tp / denom
    }
}

pub fn npv(tn: f64, fn_: f64) -> f64 {
    let denom = tn + fn_;
    if denom == 0.0 {
        f64::NAN
    } else {
        tn / denom
    }
}

pub fn lr_positive(sens: f64, spec: f64) -> f64 {
    if 1.0 - spec == 0.0 {
        f64::INFINITY
    } else {
        sens / (1.0 - spec)
    }
}

pub fn lr_negative(sens: f64, spec: f64) -> f64 {
    if sens == 0.0 {
        f64::INFINITY
    } else {
        (1.0 - spec) / sens
    }
}
