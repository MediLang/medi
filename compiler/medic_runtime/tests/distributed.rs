#![cfg(feature = "distributed")]

use medic_runtime::distributed::{
    accept_one, bind_listener, EncryptedLink, IsolatedCell, IsolationDomain, Message,
    PrivacyBoundary, PrivacyLevel,
};
use std::thread;

#[test]
fn secure_task_isolation_enforces_domain() {
    let d1 = IsolationDomain::new();
    let d2 = IsolationDomain::new();
    let cell = IsolatedCell::new(&d1, 0u32);

    assert!(cell
        .with(&d1, |v| {
            *v += 1;
        })
        .is_ok());

    let err = cell.with(&d2, |_v| {}).unwrap_err();
    assert!(format!("{err}").contains("isolation domain"));
}

#[test]
fn encrypted_inter_node_communication_round_trip() {
    let listener = bind_listener("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();

    let key = [7u8; 32];
    let boundary = PrivacyBoundary {
        max_outbound: PrivacyLevel::Phi,
    };

    let server = thread::spawn(move || {
        let stream = accept_one(&listener).unwrap();
        let mut link = EncryptedLink::from_stream(stream, key, boundary);
        let msg = link.recv().unwrap();
        assert_eq!(msg.privacy, PrivacyLevel::Internal);
        assert_eq!(msg.payload, b"hello".to_vec());

        link.send(&Message {
            privacy: PrivacyLevel::Public,
            payload: b"ok".to_vec(),
        })
        .unwrap();
    });

    let mut client = EncryptedLink::connect(&addr.to_string(), key, boundary).unwrap();
    client
        .send(&Message {
            privacy: PrivacyLevel::Internal,
            payload: b"hello".to_vec(),
        })
        .unwrap();
    let reply = client.recv().unwrap();
    assert_eq!(reply.privacy, PrivacyLevel::Public);
    assert_eq!(reply.payload, b"ok".to_vec());

    server.join().unwrap();
}

#[test]
fn privacy_boundary_enforcement_blocks_phi() {
    let listener = bind_listener("127.0.0.1:0").unwrap();
    let addr = listener.local_addr().unwrap();

    let key = [9u8; 32];

    let server_boundary = PrivacyBoundary {
        max_outbound: PrivacyLevel::Phi,
    };

    let client_boundary = PrivacyBoundary {
        max_outbound: PrivacyLevel::Internal,
    };

    let server = thread::spawn(move || {
        let stream = accept_one(&listener).unwrap();
        let mut link = EncryptedLink::from_stream(stream, key, server_boundary);
        let msg = link.recv().unwrap();
        assert_eq!(msg.privacy, PrivacyLevel::Public);
    });

    let mut client = EncryptedLink::connect(&addr.to_string(), key, client_boundary).unwrap();
    let err = client
        .send(&Message {
            privacy: PrivacyLevel::Phi,
            payload: b"phi".to_vec(),
        })
        .unwrap_err();
    assert!(format!("{err}").contains("privacy boundary"));

    client
        .send(&Message {
            privacy: PrivacyLevel::Public,
            payload: b"public".to_vec(),
        })
        .unwrap();

    server.join().unwrap();
}
