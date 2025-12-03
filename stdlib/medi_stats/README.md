# medi_stats

Statistical helpers for Medi: descriptive stats, Welch t-test, streaming stats, survival analysis, time-series, and clinical measures.

## Quick start

```rust
use medi_stats::{mean, median, stddev_sample, t_test_welch};

let a = [1.0, 2.0, 3.0, 4.0];
let b = [2.0, 3.0, 4.0, 5.0];
assert_eq!(mean(&a), 2.5);
let res = t_test_welch(&a, &b);
println!("t={:.3} df={:.1}", res.t_stat, res.df);
```

## Examples

See `examples/` for runnable programs.
