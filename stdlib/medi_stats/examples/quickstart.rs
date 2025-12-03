use medi_stats::{mean, median, stddev_sample, t_test_welch};

fn main() {
    let a = [1.0, 2.0, 3.0, 4.0];
    let b = [2.0, 3.0, 4.0, 5.0];
    println!("mean(a)={}", mean(&a));
    println!("median(a)={}", median(a.to_vec()));
    println!("stddev(a)={}", stddev_sample(&a));
    let res = t_test_welch(&a, &b);
    println!("t={:.3} df={:.1}", res.t_stat, res.df);
}
