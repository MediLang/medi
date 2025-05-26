use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use lazy_static::lazy_static;

lazy_static! {
    static ref INTERNER: Mutex<HashMap<Arc<str>, Arc<str>>> = Mutex::new(HashMap::new());
}

/// A reference-counted, interned string
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Arc<str>);

impl InternedString {
    /// Create a new interned string
    pub fn new(s: &str) -> Self {
        let mut map = INTERNER.lock().unwrap();
        match map.get(s) {
            Some(existing) => InternedString(Arc::clone(existing)),
            None => {
                let arc = Arc::from(s);
                map.insert(Arc::clone(&arc), Arc::clone(&arc));
                InternedString(arc)
            }
        }
    }

    /// Get the string slice
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<&str> for InternedString {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl std::fmt::Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_interning() {
        let s1 = InternedString::new("test");
        let s2 = InternedString::new("test");
        
        // Both should point to the same underlying string
        assert!(Arc::ptr_eq(&s1.0, &s2.0));
        
        // Different strings should be different
        let s3 = InternedString::new("different");
        assert!(!Arc::ptr_eq(&s1.0, &s3.0));
    }
}
