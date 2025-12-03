# Getting Started: medi_stats

Basic statistics and Welch t-test.

```rust
use medi_stats::{mean, stddev_sample, t_test_welch};
let a = [1.0, 2.0, 3.0];
let b = [2.0, 3.0, 4.0];
let m = mean(&a);
let s = stddev_sample(&a);
let res = t_test_welch(&a, &b);
```
