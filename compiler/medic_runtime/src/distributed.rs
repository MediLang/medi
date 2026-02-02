use crate::RuntimeError;
use aes_gcm::{
    aead::{Aead, KeyInit},
    Aes256Gcm, Key, Nonce,
};
use rand_core::{OsRng, RngCore};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Mutex;

static NEXT_DOMAIN_ID: AtomicU64 = AtomicU64::new(1);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct IsolationDomain {
    id: u64,
}

impl IsolationDomain {
    pub fn new() -> Self {
        let id = NEXT_DOMAIN_ID.fetch_add(1, Ordering::Relaxed);
        Self { id }
    }

    #[inline]
    pub fn id(&self) -> u64 {
        self.id
    }
}

impl Default for IsolationDomain {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IsolatedCell<T> {
    domain: u64,
    value: Mutex<T>,
}

impl<T> IsolatedCell<T> {
    pub fn new(domain: &IsolationDomain, value: T) -> Self {
        Self {
            domain: domain.id,
            value: Mutex::new(value),
        }
    }

    pub fn with<R>(
        &self,
        domain: &IsolationDomain,
        f: impl FnOnce(&mut T) -> R,
    ) -> Result<R, RuntimeError> {
        if domain.id != self.domain {
            return Err(RuntimeError::Message("isolation domain violation".into()));
        }
        let mut guard = self
            .value
            .lock()
            .map_err(|_| RuntimeError::Message("isolated cell mutex poisoned".into()))?;
        Ok(f(&mut guard))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum PrivacyLevel {
    Public,
    Internal,
    Phi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PrivacyBoundary {
    pub max_outbound: PrivacyLevel,
}

impl PrivacyBoundary {
    #[inline]
    pub fn allows(&self, lvl: PrivacyLevel) -> bool {
        lvl <= self.max_outbound
    }
}

#[derive(Debug, Clone)]
pub struct Message {
    pub privacy: PrivacyLevel,
    pub payload: Vec<u8>,
}

fn cipher_from_key(key_bytes: [u8; 32]) -> Aes256Gcm {
    let key = *Key::<Aes256Gcm>::from_slice(&key_bytes);
    Aes256Gcm::new(&key)
}

fn write_u32_be(w: &mut impl Write, v: u32) -> std::io::Result<()> {
    w.write_all(&v.to_be_bytes())
}

fn read_u32_be(r: &mut impl Read) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(u32::from_be_bytes(buf))
}

fn write_all(w: &mut TcpStream, bytes: &[u8]) -> Result<(), RuntimeError> {
    w.write_all(bytes)
        .map_err(|e| RuntimeError::Message(format!("tcp write: {e}")))
}

fn read_exact(r: &mut TcpStream, bytes: &mut [u8]) -> Result<(), RuntimeError> {
    r.read_exact(bytes)
        .map_err(|e| RuntimeError::Message(format!("tcp read: {e}")))
}

fn send_frame(
    stream: &mut TcpStream,
    plaintext: &[u8],
    key_bytes: [u8; 32],
) -> Result<(), RuntimeError> {
    let cipher = cipher_from_key(key_bytes);
    let mut nonce_bytes = [0u8; 12];
    OsRng.fill_bytes(&mut nonce_bytes);
    let nonce = Nonce::from_slice(&nonce_bytes);
    let ct = cipher
        .encrypt(nonce, plaintext)
        .map_err(|e| RuntimeError::Message(format!("encrypt: {e}")))?;

    let total_len = (nonce_bytes.len() + ct.len())
        .try_into()
        .map_err(|_| RuntimeError::Message("frame too large".into()))?;

    let mut header = Vec::with_capacity(4);
    write_u32_be(&mut header, total_len)
        .map_err(|e| RuntimeError::Message(format!("tcp header: {e}")))?;
    write_all(stream, &header)?;
    write_all(stream, &nonce_bytes)?;
    write_all(stream, &ct)?;
    Ok(())
}

fn recv_frame(stream: &mut TcpStream, key_bytes: [u8; 32]) -> Result<Vec<u8>, RuntimeError> {
    let mut header = [0u8; 4];
    read_exact(stream, &mut header)?;
    let len = u32::from_be_bytes(header) as usize;
    if len < 13 {
        return Err(RuntimeError::Message("ciphertext frame too short".into()));
    }
    let mut buf = vec![0u8; len];
    read_exact(stream, &mut buf)?;
    let (nonce_bytes, ct) = buf.split_at(12);

    let cipher = cipher_from_key(key_bytes);
    let nonce = Nonce::from_slice(nonce_bytes);
    cipher
        .decrypt(nonce, ct)
        .map_err(|e| RuntimeError::Message(format!("decrypt: {e}")))
}

pub struct EncryptedLink {
    stream: TcpStream,
    key: [u8; 32],
    boundary: PrivacyBoundary,
}

impl EncryptedLink {
    pub fn connect(
        addr: &str,
        key: [u8; 32],
        boundary: PrivacyBoundary,
    ) -> Result<Self, RuntimeError> {
        let stream = TcpStream::connect(addr)
            .map_err(|e| RuntimeError::Message(format!("tcp connect {addr}: {e}")))?;
        Ok(Self {
            stream,
            key,
            boundary,
        })
    }

    pub fn from_stream(stream: TcpStream, key: [u8; 32], boundary: PrivacyBoundary) -> Self {
        Self {
            stream,
            key,
            boundary,
        }
    }

    pub fn send(&mut self, msg: &Message) -> Result<(), RuntimeError> {
        if !self.boundary.allows(msg.privacy) {
            return Err(RuntimeError::Message("privacy boundary violation".into()));
        }
        let level_byte = match msg.privacy {
            PrivacyLevel::Public => 0u8,
            PrivacyLevel::Internal => 1u8,
            PrivacyLevel::Phi => 2u8,
        };
        let mut payload = Vec::with_capacity(1 + msg.payload.len());
        payload.push(level_byte);
        payload.extend_from_slice(&msg.payload);
        send_frame(&mut self.stream, &payload, self.key)
    }

    pub fn recv(&mut self) -> Result<Message, RuntimeError> {
        let pt = recv_frame(&mut self.stream, self.key)?;
        if pt.is_empty() {
            return Err(RuntimeError::Message("empty frame".into()));
        }
        let privacy = match pt[0] {
            0 => PrivacyLevel::Public,
            1 => PrivacyLevel::Internal,
            2 => PrivacyLevel::Phi,
            _ => return Err(RuntimeError::Message("unknown privacy level".into())),
        };
        Ok(Message {
            privacy,
            payload: pt[1..].to_vec(),
        })
    }
}

pub fn bind_listener(addr: &str) -> Result<TcpListener, RuntimeError> {
    TcpListener::bind(addr).map_err(|e| RuntimeError::Message(format!("tcp bind {addr}: {e}")))
}

pub fn accept_one(listener: &TcpListener) -> Result<TcpStream, RuntimeError> {
    let (stream, _peer) = listener
        .accept()
        .map_err(|e| RuntimeError::Message(format!("tcp accept: {e}")))?;
    Ok(stream)
}
