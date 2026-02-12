use tlvxc_runtime::gc::{GarbageCollector, GcParams};

#[derive(Clone)]
#[allow(dead_code)]
struct PatientLike {
    name: String,
    birth_date: String,
    // references to related resources (e.g., encounters, observations)
    links: Vec<u64>,
}

#[derive(Clone)]
#[allow(dead_code)]
struct ObservationLike {
    code: String,
    value: String,
    subject: u64,
}

fn mk_patient(i: u32) -> PatientLike {
    PatientLike {
        name: format!("Patient-{i}"),
        birth_date: format!("19{:02}-{:02}-{:02}", (i % 100), (i % 12) + 1, (i % 28) + 1),
        links: Vec::new(),
    }
}

fn mk_observation(i: u32, subject: u64) -> ObservationLike {
    ObservationLike {
        code: format!("LOINC:{:05}", 10000 + (i % 5000)),
        value: format!("{:0.2}", (i as f64) * 0.1),
        subject,
    }
}

#[test]
fn gc_healthcare_like_workload_stress() {
    // Targets:
    // - churn many small objects (strings/structs)
    // - maintain a working set of roots to simulate "active patient panel"
    // - create edges to simulate resource graphs
    // - exercise minor GC thresholds and (optionally) incremental major GC
    let mut gc = GarbageCollector::with_params(GcParams {
        nursery_threshold_bytes: 1 << 16, // 64KiB to force frequent minors
        gen_promotion_threshold: 2,
        max_pause_ms: 5,
    });

    let mut active_patients: Vec<u64> = Vec::new();

    // Create initial working set
    for i in 0..512u32 {
        let p = gc.allocate(mk_patient(i));
        gc.add_root(&p);
        active_patients.push(p.id());
        // Attach a few observations to each patient
        for j in 0..6u32 {
            let o = gc.allocate(mk_observation(i * 10 + j, p.id() as u64));
            gc.add_edge(p.id(), o.id());
        }
    }

    // Main churn loop: create + drop temporary resources, rotate active roots.
    for t in 0..20_000u32 {
        // Rotate working set: occasionally unroot an older patient and root a new one.
        if t % 37 == 0 {
            if let Some(old_id) = active_patients.first().copied() {
                gc.remove_root_id(old_id);
                active_patients.remove(0);
            }
            let new_p = gc.allocate(mk_patient(10_000 + t));
            gc.add_root(&new_p);
            active_patients.push(new_p.id());
        }

        // Temporary allocations simulating query results / transforms.
        let temp_patient = gc.allocate(mk_patient(50_000 + t));
        let root_temp = (t % 200) == 0;
        if root_temp {
            gc.add_root(&temp_patient);
        }

        for k in 0..3u32 {
            let o = gc.allocate(mk_observation(t * 3 + k, temp_patient.id() as u64));
            gc.add_edge(temp_patient.id(), o.id());
        }

        if root_temp {
            gc.remove_root(&temp_patient);
        }

        if gc.should_minor_collect() {
            gc.minor_collect();
        }

        #[cfg(feature = "gc-incremental")]
        {
            gc.maybe_start_incremental_major();
            if gc.incremental_is_active() {
                let _ = gc.incremental_step_with_params();
            }
        }

        // Occasionally do a full major collection to emulate "idle" cleanup.
        if t % 5000 == 0 {
            gc.collect_garbage();
        }
    }

    // Final cleanup: drop all roots and ensure we can return near baseline.
    for id in active_patients {
        gc.remove_root_id(id);
    }
    gc.collect_garbage();

    let st = gc.stats();
    assert!(
        st.total_objects < 5000,
        "expected most objects to be collectable after dropping roots; total_objects={}",
        st.total_objects
    );
}
