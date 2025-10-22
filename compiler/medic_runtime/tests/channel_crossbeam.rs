use medic_runtime::{create_bounded_channel, create_unbounded_channel};
use std::thread;

#[test]
fn xchan_unbounded_roundtrip() {
    let (tx, rx) = create_unbounded_channel::<usize>();
    let h = thread::spawn(move || {
        for i in 0..10_000 {
            tx.send(i).unwrap();
        }
    });
    let mut sum = 0usize;
    for _ in 0..10_000 {
        sum += rx.recv().unwrap();
    }
    h.join().unwrap();
    assert_eq!(sum, (0..10_000).sum());
}

#[test]
fn xchan_bounded_backpressure() {
    let (tx, rx) = create_bounded_channel::<usize>(64);

    // Producer thread: will block occasionally due to small capacity
    let prod = thread::spawn(move || {
        for i in 0..2_000 {
            // Use blocking send to exercise backpressure
            tx.send(i).unwrap();
        }
    });

    // Consumer thread
    let cons = thread::spawn(move || {
        let mut last = 0usize;
        for i in 0..2_000 {
            let v = rx.recv().unwrap();
            if i > 0 {
                assert!(v > last);
            }
            last = v;
        }
    });

    prod.join().unwrap();
    cons.join().unwrap();
}
