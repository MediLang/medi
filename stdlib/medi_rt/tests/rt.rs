use std::time::Duration;

use medi_rt::{
    device::{parse_device_message, DeviceProtocol},
    io::{set_low_latency, tcp_connect_timeout},
    stream::{sliding_window_sum, RingBuffer},
};

#[test]
fn ring_buffer_drops_oldest_when_full() {
    let mut rb = RingBuffer::with_capacity(2);
    rb.push(1);
    rb.push(2);
    rb.push(3);

    assert_eq!(rb.dropped(), 1);
    assert_eq!(rb.pop(), Some(2));
    assert_eq!(rb.pop(), Some(3));
    assert_eq!(rb.pop(), None);
}

#[test]
fn sliding_window_sum_basic() {
    let xs = vec![1.0, 2.0, 3.0, 4.0];
    assert_eq!(sliding_window_sum(&xs, 2), vec![3.0, 5.0, 7.0]);
    assert!(sliding_window_sum(&xs, 0).is_empty());
}

#[test]
fn parse_device_message_csv() {
    let r = parse_device_message(DeviceProtocol::Hl7, "dev1,hr,72.5,1000").expect("reading");
    assert_eq!(r.device_id, "dev1");
    assert_eq!(r.metric, "hr");
    assert_eq!(r.value, 72.5);
    assert_eq!(r.timestamp_ms, 1000);
}

#[test]
fn tcp_helpers_compile_and_are_callable() {
    // We don't require a live server in unit tests; just ensure APIs work.
    // Connecting to localhost:0 should fail quickly.
    let _ = tcp_connect_timeout(("127.0.0.1", 0), Duration::from_millis(10)).err();

    // And we can call set_low_latency on a stream if we had one; here just ensure it typechecks.
    // (No actual stream available without opening a listener.)
    fn _typecheck_only(s: &std::net::TcpStream) {
        let _ = set_low_latency(s);
    }
}
