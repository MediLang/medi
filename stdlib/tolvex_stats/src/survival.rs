//! Survival analysis: Kaplan–Meier estimator and log-rank test (simplified)

/// Kaplan–Meier output
#[derive(Debug, Clone)]
pub struct KaplanMeier {
    pub times: Vec<f64>,
    pub survival_prob: Vec<f64>,
    pub at_risk: Vec<usize>,
    pub events: Vec<usize>,
}

/// Compute Kaplan–Meier survival curve.
/// times: event/censor times; events: true if event occurred, false if censored.
pub fn kaplan_meier(times: &[f64], events: &[bool]) -> KaplanMeier {
    assert_eq!(times.len(), events.len());
    let n = times.len();
    if n == 0 {
        return KaplanMeier {
            times: vec![],
            survival_prob: vec![],
            at_risk: vec![],
            events: vec![],
        };
    }
    // Combine and sort by time
    let mut data: Vec<(f64, bool)> = times.iter().cloned().zip(events.iter().cloned()).collect();
    data.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

    let mut uniq_times: Vec<f64> = Vec::new();
    let mut d: Vec<usize> = Vec::new(); // events
    let mut n_risk: Vec<usize> = Vec::new(); // at risk just prior

    let mut i = 0;
    while i < n {
        let t = data[i].0;
        let at_risk = n - i; // number still under observation just prior to t
        let mut events_t = 0usize;
        let mut j = i;
        while j < n && data[j].0 == t {
            if data[j].1 {
                events_t += 1;
            }
            j += 1;
        }
        uniq_times.push(t);
        d.push(events_t);
        n_risk.push(at_risk);
        i = j;
    }

    let mut s = 1.0f64;
    let mut surv: Vec<f64> = Vec::with_capacity(uniq_times.len());
    for (&ni, &di) in n_risk.iter().zip(d.iter()) {
        if ni == 0 {
            surv.push(s);
            continue;
        }
        let frac = 1.0 - (di as f64) / (ni as f64);
        s *= frac.max(0.0);
        surv.push(s);
    }

    KaplanMeier {
        times: uniq_times,
        survival_prob: surv,
        at_risk: n_risk,
        events: d,
    }
}

/// Simplified log-rank test (returns chi-square statistic p-value using approx. normal/chi-square)
/// For robustness and to avoid extra deps, we return the chi-square statistic only.
/// Caller can compare against critical values or compute p-value externally if desired.
pub fn log_rank_test_stat(
    group1_times: &[f64],
    group1_events: &[bool],
    group2_times: &[f64],
    group2_events: &[bool],
) -> f64 {
    assert_eq!(group1_times.len(), group1_events.len());
    assert_eq!(group2_times.len(), group2_events.len());
    // Merge unique times across groups
    let mut all: Vec<(f64, usize, bool)> = Vec::new();
    for i in 0..group1_times.len() {
        all.push((group1_times[i], 0, group1_events[i]));
    }
    for i in 0..group2_times.len() {
        all.push((group2_times[i], 1, group2_events[i]));
    }
    all.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());

    // At each unique time t with events, compute observed and expected in group 1
    let mut uniq_times: Vec<f64> = Vec::new();
    let mut d1_t: Vec<usize> = Vec::new();
    let mut d_t: Vec<usize> = Vec::new();
    let mut n1_t: Vec<usize> = Vec::new();
    let mut n_t: Vec<usize> = Vec::new();

    let mut i = 0;
    while i < all.len() {
        let t = all[i].0;
        let mut d1 = 0usize;
        let mut d = 0usize;
        let mut n1 = 0usize;
        let mut n = 0usize;
        // Count at risk just prior to t
        for item in all.iter().skip(i) {
            // at risk: those with time >= t
            n += 1;
            if item.1 == 0 {
                n1 += 1;
            }
        }
        // Events at t
        let mut j = i;
        while j < all.len() && all[j].0 == t {
            if all[j].2 {
                d += 1;
                if all[j].1 == 0 {
                    d1 += 1;
                }
            }
            j += 1;
        }
        if d > 0 {
            uniq_times.push(t);
            d1_t.push(d1);
            d_t.push(d);
            n1_t.push(n1);
            n_t.push(n);
        }
        i = j;
    }

    // Compute log-rank statistic: (sum (d1 - E1))^2 / sum Var(d1)
    let mut num = 0.0f64;
    let mut den = 0.0f64;
    for k in 0..uniq_times.len() {
        let n = n_t[k] as f64;
        let n1 = n1_t[k] as f64;
        let d = d_t[k] as f64;
        let d1 = d1_t[k] as f64;
        if n == 0.0 {
            continue;
        }
        let e1 = d * (n1 / n);
        let v1 = d * (n - d) * (n1 / n) * (1.0 - n1 / n) / (n - 1.0).max(1.0);
        num += d1 - e1;
        den += v1;
    }
    if den <= 0.0 {
        0.0
    } else {
        (num * num) / den
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn km_survival_monotone() {
        let times = vec![1.0, 2.0, 2.0, 3.0, 5.0];
        let events = vec![true, false, true, true, false];
        let km = kaplan_meier(&times, &events);
        for w in km.survival_prob.windows(2) {
            assert!(w[1] <= w[0] + 1e-12);
        }
        assert_eq!(km.times.len(), km.survival_prob.len());
    }

    #[test]
    fn log_rank_basic_nonnegative() {
        // Simple synthetic datasets
        let t1 = vec![1.0, 2.0, 3.0, 4.0];
        let e1 = vec![true, true, false, true];
        let t2 = vec![1.5, 2.5, 3.5, 4.5];
        let e2 = vec![true, false, true, false];
        let stat = log_rank_test_stat(&t1, &e1, &t2, &e2);
        assert!(stat >= 0.0);
    }
}
