#[cfg(feature = "encryption-aes-gcm")]
mod imp {
    use aes_gcm::{
        aead::{Aead, KeyInit, OsRng},
        Aes256Gcm, Key, Nonce,
    };
    use serde::{de::DeserializeOwned, Serialize};

    use crate::storage::{SecureStore, StoreError};

    pub struct AesGcmStore {
        key: Key<Aes256Gcm>,
    }

    #[allow(deprecated)]
    impl AesGcmStore {
        pub fn new_random() -> Self {
            let key = Aes256Gcm::generate_key(&mut OsRng);
            Self { key }
        }

        pub fn from_bytes(key_bytes: [u8; 32]) -> Self {
            let key = *Key::<Aes256Gcm>::from_slice(&key_bytes);
            Self { key }
        }

        fn cipher(&self) -> Aes256Gcm {
            Aes256Gcm::new(&self.key)
        }
    }

    #[allow(deprecated)]
    impl SecureStore for AesGcmStore {
        fn save<T: Serialize>(&self, _key: &str, _value: &T) -> Result<(), StoreError> {
            // Placeholder: A real implementation would persist ciphertext to a backend.
            // We just exercise encrypt/decrypt paths.
            let plaintext =
                serde_json::to_vec(_value).map_err(|e| StoreError::new(e.to_string()))?;
            let nonce = Nonce::from_slice(&[0u8; 12]);
            let _ciphertext = self
                .cipher()
                .encrypt(nonce, plaintext.as_ref())
                .map_err(|e| StoreError::new(format!("encrypt error: {e}")))?;
            Ok(())
        }

        fn load<T: DeserializeOwned>(&self, _key: &str) -> Result<Option<T>, StoreError> {
            // Placeholder returns None; not persisted in-memory here
            Ok(None)
        }

        fn list_keys(&self) -> Result<Vec<String>, StoreError> {
            // Placeholder: no persistence in this stub
            Ok(vec![])
        }

        fn remove(&self, _key: &str) -> Result<(), StoreError> {
            // Placeholder: nothing to remove in this stub
            Ok(())
        }
    }

    pub use AesGcmStore as Store;
}

#[cfg(feature = "encryption-aes-gcm")]
pub use imp::Store as AesGcmStore;
