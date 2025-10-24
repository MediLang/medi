//! Example: IoT sensor loop using FixedPool and RtRegion (requires --features rt_zones)
//! Run: cargo run -p medic_runtime --example rt_iot --features rt_zones

#[cfg(feature = "rt_zones")]
fn main() {
    use medic_runtime::rt::RtZone;
    use std::time::{Duration, Instant};

    #[derive(Clone, Copy, Debug)]
    struct Msg {
        sensor_id: u16,
        value: u16,
    }

    const NMSGS: usize = 256;
    const RBUF: usize = 8 * 1024;

    let zone: RtZone<Msg, RBUF, NMSGS> = RtZone::new();

    let iterations = 2000usize;
    let mut worst = Duration::ZERO;

    for i in 0..iterations {
        let start = Instant::now();

        let scope = zone.scope();
        // Acquire message from pool (auto-return on drop)
        let m = scope.alloc_pool_box(Msg {
            sensor_id: (i % 1024) as u16,
            value: (i % 4096) as u16,
        });
        if let Some(mref) = m {
            // Transient working buffer via zone scope; auto reset on drop
            let buf = scope.alloc_region_array_uninit::<u8>(64).unwrap();
            let acc = (mref.sensor_id as u32) * 31 + (mref.value as u32) * 7;
            let b = (acc & 0xFF) as u8;
            unsafe {
                let p = buf.as_ptr() as *mut u8;
                p.write_volatile(b);
            }
            // mref dropped here -> auto return to pool
        }

        let d = start.elapsed();
        if d > worst {
            worst = d;
        }

        // Region memory is reclaimed each scope via RtZoneScope drop
    }

    println!(
        "Completed {iterations} iterations. Worst-case op latency: {:?}",
        worst
    );
}

#[cfg(not(feature = "rt_zones"))]
fn main() {
    eprintln!("This example requires --features rt_zones");
}
