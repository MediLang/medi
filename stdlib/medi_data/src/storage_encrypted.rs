#![cfg(feature = "encryption-aes-gcm")]
use std::fs::{create_dir_all, read, write};
use std::path::{Path, PathBuf};

use aes_gcm::{
    aead::{Aead, KeyInit},
    Aes256Gcm, Key, Nonce,
};
use rand_core::{OsRng, RngCore};
use serde::{de::DeserializeOwned, Serialize};

use crate::storage::{SecureStore, StoreError};

/// AES-GCM encrypted store that persists ciphertext files with ".enc" extension.
/// File format: [12-byte nonce][ciphertext bytes]
pub struct EncryptedFileStore {
    dir: PathBuf,
    key: Key<Aes256Gcm>,
}

impl EncryptedFileStore {
    pub fn new(dir: impl AsRef<Path>, key_bytes: [u8; 32]) -> Result<Self, StoreError> {
        let dir = dir.as_ref().to_path_buf();
        if !dir.exists() {
            create_dir_all(&dir).map_err(|e| StoreError::new(e.to_string()))?;
        }
        let key = Key::<Aes256Gcm>::from_slice(&key_bytes).clone();
        Ok(Self { dir, key })
    }

    pub fn from_env(dir: impl AsRef<Path>, env_key_var: &str) -> Result<Self, StoreError> {
        let key_hex = std::env::var(env_key_var)
            .map_err(|e| StoreError::new(format!("env {}: {}", env_key_var, e)))?;
        let key_bytes_vec =
            hex::decode(key_hex).map_err(|e| StoreError::new(format!("hex decode: {}", e)))?;
        if key_bytes_vec.len() != 32 {
            return Err(StoreError::new("AES-256-GCM key must be 32 bytes"));
        }
        let mut arr = [0u8; 32];
        arr.copy_from_slice(&key_bytes_vec);
        Self::new(dir, arr)
    }

    fn path_for(&self, key: &str) -> PathBuf {
        self.dir.join(format!("{}.enc", key))
    }
    fn cipher(&self) -> Aes256Gcm {
        Aes256Gcm::new(&self.key)
    }
}

impl SecureStore for EncryptedFileStore {
    fn save<T: Serialize>(&self, key: &str, value: &T) -> Result<(), StoreError> {
        let path = self.path_for(key);
        let plaintext = serde_json::to_vec(value).map_err(|e| StoreError::new(e.to_string()))?;
        // random nonce per record
        let mut nonce_bytes = [0u8; 12];
        OsRng.fill_bytes(&mut nonce_bytes);
        let nonce = Nonce::from_slice(&nonce_bytes);
        let ct = self
            .cipher()
            .encrypt(nonce, plaintext.as_ref())
            .map_err(|e| StoreError::new(format!("encrypt: {}", e)))?;
        let mut out = Vec::with_capacity(12 + ct.len());
        out.extend_from_slice(&nonce_bytes);
        out.extend_from_slice(&ct);
        write(&path, out).map_err(|e| StoreError::new(format!("write {}: {}", path.display(), e)))
    }

    fn load<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>, StoreError> {
        let path = self.path_for(key);
        if !path.exists() {
            return Ok(None);
        }
        let data =
            read(&path).map_err(|e| StoreError::new(format!("read {}: {}", path.display(), e)))?;
        if data.len() < 13 {
            return Err(StoreError::new("ciphertext too short"));
        }
        let (nonce_bytes, ct) = data.split_at(12);
        let nonce = Nonce::from_slice(nonce_bytes);
        let pt = self
            .cipher()
            .decrypt(nonce, ct)
            .map_err(|e| StoreError::new(format!("decrypt: {}", e)))?;
        let v = serde_json::from_slice::<T>(&pt).map_err(|e| StoreError::new(e.to_string()))?;
        Ok(Some(v))
    }

    fn list_keys(&self) -> Result<Vec<String>, StoreError> {
        let mut keys = Vec::new();
        for entry in std::fs::read_dir(&self.dir)
            .map_err(|e| StoreError::new(format!("read_dir {}: {}", self.dir.display(), e)))?
        {
            let entry = entry.map_err(|e| StoreError::new(e.to_string()))?;
            let path = entry.path();
            if path.is_file() {
                if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
                    if let Some(stripped) = name.strip_suffix(".enc") {
                        keys.push(stripped.to_string());
                    }
                }
            }
        }
        Ok(keys)
    }

    fn remove(&self, key: &str) -> Result<(), StoreError> {
        let path = self.path_for(key);
        if path.exists() {
            std::fs::remove_file(&path)
                .map_err(|e| StoreError::new(format!("remove {}: {}", path.display(), e)))?;
        }
        Ok(())
    }
}
