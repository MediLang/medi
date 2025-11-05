use serde::{de::DeserializeOwned, Serialize};

pub trait SecureStore {
    fn save<T: Serialize>(&self, _key: &str, _value: &T) -> Result<(), StoreError>;
    fn load<T: DeserializeOwned>(&self, _key: &str) -> Result<Option<T>, StoreError>;
    fn list_keys(&self) -> Result<Vec<String>, StoreError>;
    fn remove(&self, _key: &str) -> Result<(), StoreError>;
}

#[derive(Debug)]
pub struct StoreError {
    pub message: String,
}

impl StoreError {
    pub fn new(msg: impl Into<String>) -> Self {
        Self {
            message: msg.into(),
        }
    }
}

#[derive(Default)]
pub struct InMemoryStore;

impl SecureStore for InMemoryStore {
    fn save<T: Serialize>(&self, _key: &str, _value: &T) -> Result<(), StoreError> {
        Ok(())
    }
    fn load<T: DeserializeOwned>(&self, _key: &str) -> Result<Option<T>, StoreError> {
        Ok(None)
    }
    fn list_keys(&self) -> Result<Vec<String>, StoreError> {
        Ok(vec![])
    }
    fn remove(&self, _key: &str) -> Result<(), StoreError> {
        Ok(())
    }
}
