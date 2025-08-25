use lazy_static::lazy_static;
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

lazy_static! {
    static ref GLOBAL_INTERNER: Mutex<StringInterner> = Mutex::new(StringInterner::default());
}

// Serde support: serialize as a plain string, and deserialize by reinterning.
#[cfg(feature = "serde")]
impl Serialize for InternedString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

#[cfg(feature = "serde")]
impl<'de> Deserialize<'de> for InternedString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Ok(InternedString::from_string(s))
    }
}

/// A thread-safe string interner for deduplicating string literals and identifiers
#[derive(Debug)]
pub struct StringInterner {
    inner: Mutex<StringInternerInner>,
}

impl StringInterner {
    /// Create a new StringInterner with the specified maximum size
    pub fn new(max_size: usize) -> Self {
        Self {
            inner: Mutex::new(StringInternerInner {
                map: HashMap::new(),
                max_size,
            }),
        }
    }

    /// Get an interned string, or insert it if it doesn't exist
    pub fn get_or_insert(&self, s: &str) -> Arc<str> {
        let mut inner = self.inner.lock().unwrap();
        inner.get_or_insert(s)
    }

    /// Get or insert a string using the global interner
    pub fn get_or_insert_global(s: &str) -> InternedString {
        // First acquire the outer mutex, then delegate.
        let interner = GLOBAL_INTERNER
            .lock()
            .expect("String interner mutex poisoned");
        // `StringInterner::get_or_insert` will lock the inner map
        // (double-lock but still safe on the same thread).
        let arc = interner.get_or_insert(s);
        InternedString::from_arc(arc)
    }
}

impl Default for StringInterner {
    fn default() -> Self {
        Self::new(10_000)
    }
}

#[derive(Debug)]
struct StringInternerInner {
    map: HashMap<Arc<str>, Arc<str>>,
    max_size: usize,
}

impl StringInternerInner {
    fn get_or_insert(&mut self, s: &str) -> Arc<str> {
        if let Some(existing) = self.map.get(s) {
            return Arc::clone(existing);
        }

        // If we're at capacity, clear the cache (simple strategy)
        if self.map.len() >= self.max_size {
            self.map.clear();
        }

        let arc = Arc::from(s);
        self.map.insert(Arc::clone(&arc), Arc::clone(&arc));
        arc
    }
}

/// A reference-counted, interned string
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InternedString(Arc<str>);

impl InternedString {
    /// Creates a new interned string from a string slice.
    pub fn new(s: &str) -> Self {
        let interner = GLOBAL_INTERNER.lock().unwrap();
        let arc = interner.get_or_insert(s);
        Self(arc)
    }

    /// Creates a new interned string from an owned string.
    pub fn from_string(s: String) -> Self {
        let interner = GLOBAL_INTERNER.lock().unwrap();
        let arc = interner.get_or_insert(&s);
        Self(arc)
    }

    /// Creates a new interned string from a static string.
    /// This is not a const function because Arc::from() isn't const.
    /// If you need a const function, consider using lazy_static or once_cell for initialization.
    pub fn from_static(s: &'static str) -> Self {
        // Use the global interner to deduplicate the string
        let interner = GLOBAL_INTERNER.lock().unwrap();
        let arc = interner.get_or_insert(s);
        InternedString(arc)
    }

    /// Returns the string slice of the interned string.
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Returns the length of the string in bytes.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns `true` if the string has a length of zero, and `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Creates an `InternedString` from an `Arc<str>`.
    pub fn from_arc(arc: Arc<str>) -> Self {
        InternedString(arc)
    }

    /// Returns the underlying `Arc<str>`.
    pub fn into_arc(self) -> Arc<str> {
        self.0
    }
}

// Implement PartialEq for &str to allow comparison with string literals
impl PartialEq<&str> for InternedString {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
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
    use std::sync::Arc as StdArc;
    use std::sync::Barrier;
    use std::thread;

    #[test]
    fn test_string_interning() {
        let s1 = InternedString::from("test");
        let s2 = InternedString::from("test");

        // Both should point to the same underlying string
        assert!(Arc::ptr_eq(&s1.0, &s2.0));

        // Different strings should be different
        let s3 = InternedString::from("different");
        assert!(!Arc::ptr_eq(&s1.0, &s3.0));
    }

    #[test]
    fn test_empty_string() {
        let s1 = InternedString::from("");
        let s2 = InternedString::from("");
        assert!(Arc::ptr_eq(&s1.0, &s2.0));
        assert_eq!(s1.as_str(), "");
    }

    #[test]
    fn test_from_str_trait() {
        let s1 = InternedString::from("test");
        let s2 = InternedString::from("test");
        assert!(Arc::ptr_eq(&s1.0, &s2.0));
    }

    #[test]
    fn test_display_trait() {
        let s = InternedString::from("hello world");
        assert_eq!(format!("{s}"), "hello world");
    }

    #[test]
    fn test_thread_safety() {
        let num_threads = 4;
        let barrier = StdArc::new(Barrier::new(num_threads));
        let mut handles = vec![];

        // Create a shared vector to collect strings from all threads
        let shared_strings = StdArc::new(Mutex::new(Vec::new()));

        for i in 0..num_threads {
            let barrier = barrier.clone();
            let shared_strings = shared_strings.clone();
            let handle = thread::spawn(move || {
                // Wait for all threads to start
                barrier.wait();

                // Each thread interns the same strings
                let strings: Vec<_> = (0..100)
                    .map(|j| InternedString::from(format!("string_{}", j % 10).as_str()))
                    .collect();

                // Verify that identical strings are interned
                for j in 0..strings.len() - 1 {
                    for k in j + 1..strings.len() {
                        if strings[j].as_str() == strings[k].as_str() {
                            assert!(
                                Arc::ptr_eq(&strings[j].0, &strings[k].0),
                                "Thread {i}: Identical strings should share the same Arc"
                            );
                        }
                    }
                }

                // Verify that different strings don't share the same Arc
                for j in 0..strings.len() - 1 {
                    for k in j + 1..strings.len() {
                        if strings[j].as_str() != strings[k].as_str() {
                            assert!(
                                !Arc::ptr_eq(&strings[j].0, &strings[k].0),
                                "Thread {i}: Different strings should not share the same Arc"
                            );
                        }
                    }
                }

                // Verify that the strings are still valid after dropping the original Arc
                let first_string = strings[0].clone();
                std::mem::drop(strings);
                assert!(!first_string.0.is_empty());

                // Add strings to shared collection for cross-thread verification
                shared_strings.lock().unwrap().extend(
                    (0..10).map(|j| InternedString::from(format!("shared_{i}_{j}").as_str())),
                );

                // Return the first string for cross-thread verification
                first_string
            });
            handles.push(handle);
        }

        // Wait for all threads to complete and collect results
        let results: Vec<_> = handles
            .into_iter()
            .map(|h| h.join().expect("Thread panicked"))
            .collect();

        // Verify cross-thread interning worked
        let shared_strings = shared_strings.lock().unwrap();
        // Verify cross-thread interning worked with shared strings
        for i in 0..shared_strings.len() - 1 {
            for j in i + 1..shared_strings.len() {
                if shared_strings[i].as_str() == shared_strings[j].as_str() {
                    assert!(
                        Arc::ptr_eq(&shared_strings[i].0, &shared_strings[j].0),
                        "Cross-thread: Identical strings should share the same Arc"
                    );
                }
            }
        }

        // Verify that the results from all threads are valid
        for result in results {
            assert!(!result.0.is_empty());
        }
    }
}
