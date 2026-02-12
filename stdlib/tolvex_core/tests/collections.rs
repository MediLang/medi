use tolvex_core::{MediMap, MediVec};

#[test]
fn vec_basics_push_get_set_iter() {
    let mut v = MediVec::new();
    assert!(v.is_empty());

    v.push(1);
    v.push(2);
    v.push(3);

    assert_eq!(v.len(), 3);
    assert_eq!(v.get(1), Some(&2));

    assert!(v.set(1, 20).is_ok());
    assert_eq!(v.get(1), Some(&20));

    let sum: i32 = v.iter().copied().sum();
    assert_eq!(sum, 1 + 20 + 3);

    let collected: Vec<i32> = (&v).into_iter().copied().collect();
    assert_eq!(collected, vec![1, 20, 3]);
}

#[test]
fn vec_into_iter_moves_values() {
    let mut v = MediVec::new();
    v.push("a".to_string());
    v.push("b".to_string());

    let joined = v.into_iter().collect::<Vec<_>>().join(",");
    assert_eq!(joined, "a,b");
}

#[test]
fn map_basics_insert_get_iter_remove() {
    let mut m = MediMap::new();
    assert!(m.is_empty());

    assert_eq!(m.insert("a", 1), None);
    assert_eq!(m.insert("b", 2), None);
    assert_eq!(m.insert("a", 10), Some(1));

    assert_eq!(m.len(), 2);
    assert!(m.contains_key(&"a"));
    assert_eq!(m.get(&"a"), Some(&10));

    let items: Vec<(&str, i32)> = m.iter().map(|(k, v)| (*k, *v)).collect();
    assert_eq!(items, vec![("a", 10), ("b", 2)]);

    assert_eq!(m.remove(&"b"), Some(2));
    assert_eq!(m.get(&"b"), None);
}
