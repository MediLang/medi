use crate::storage::{SecureStore, StoreError};
use crate::storage_file::FileStore;

#[cfg(feature = "encryption-aes-gcm")]
use crate::storage_encrypted::EncryptedFileStore;

/// Composite store that delegates to FileStore or EncryptedFileStore
pub enum CompositeStore {
    File(FileStore),
    #[cfg(feature = "encryption-aes-gcm")]
    Encrypted(EncryptedFileStore),
}

impl CompositeStore {
    pub fn from_file(store: FileStore) -> Self {
        CompositeStore::File(store)
    }
    #[cfg(feature = "encryption-aes-gcm")]
    pub fn from_encrypted(store: EncryptedFileStore) -> Self {
        CompositeStore::Encrypted(store)
    }
}

impl SecureStore for CompositeStore {
    fn save<T: serde::Serialize>(&self, key: &str, value: &T) -> Result<(), StoreError> {
        match self {
            CompositeStore::File(s) => s.save(key, value),
            #[cfg(feature = "encryption-aes-gcm")]
            CompositeStore::Encrypted(s) => s.save(key, value),
        }
    }

    fn load<T: serde::de::DeserializeOwned>(&self, key: &str) -> Result<Option<T>, StoreError> {
        match self {
            CompositeStore::File(s) => s.load(key),
            #[cfg(feature = "encryption-aes-gcm")]
            CompositeStore::Encrypted(s) => s.load(key),
        }
    }

    fn list_keys(&self) -> Result<Vec<String>, StoreError> {
        match self {
            CompositeStore::File(s) => s.list_keys(),
            #[cfg(feature = "encryption-aes-gcm")]
            CompositeStore::Encrypted(s) => s.list_keys(),
        }
    }

    fn remove(&self, key: &str) -> Result<(), StoreError> {
        match self {
            CompositeStore::File(s) => s.remove(key),
            #[cfg(feature = "encryption-aes-gcm")]
            CompositeStore::Encrypted(s) => s.remove(key),
        }
    }
}
