use serde::{de::DeserializeOwned, Serialize};
use std::fs::{create_dir_all, read, write};
use std::path::{Path, PathBuf};

use crate::storage::{SecureStore, StoreError};

#[derive(Debug, Clone)]
pub struct FileStore {
    dir: PathBuf,
}

impl FileStore {
    pub fn new<P: AsRef<Path>>(dir: P) -> Result<Self, StoreError> {
        let dir = dir.as_ref().to_path_buf();
        create_dir_all(&dir).map_err(|e| StoreError::new(format!("create_dir_all: {e}")))?;
        Ok(Self { dir })
    }

    fn path_for(&self, key: &str) -> PathBuf {
        let mut fname = String::with_capacity(key.len());
        for ch in key.chars() {
            if ch.is_ascii_alphanumeric() || ch == '-' || ch == '_' {
                fname.push(ch);
            } else {
                fname.push('_');
            }
        }
        self.dir.join(format!("{fname}.json"))
    }
}

impl SecureStore for FileStore {
    fn save<T: Serialize>(&self, key: &str, value: &T) -> Result<(), StoreError> {
        let path = self.path_for(key);
        let data = serde_json::to_vec(value).map_err(|e| StoreError::new(e.to_string()))?;
        write(&path, data).map_err(|e| StoreError::new(format!("write {}: {}", path.display(), e)))
    }

    fn load<T: DeserializeOwned>(&self, key: &str) -> Result<Option<T>, StoreError> {
        let path = self.path_for(key);
        if !path.exists() {
            return Ok(None);
        }
        let data =
            read(&path).map_err(|e| StoreError::new(format!("read {}: {}", path.display(), e)))?;
        let v = serde_json::from_slice::<T>(&data).map_err(|e| StoreError::new(e.to_string()))?;
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
                    if let Some(stripped) = name.strip_suffix(".json") {
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
