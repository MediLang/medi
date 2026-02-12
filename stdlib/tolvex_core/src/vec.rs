use std::slice;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MediVec<T> {
    inner: Vec<T>,
}

impl<T> MediVec<T> {
    #[inline]
    pub fn new() -> Self {
        Self { inner: Vec::new() }
    }

    #[inline]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            inner: Vec::with_capacity(cap),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    #[inline]
    pub fn push(&mut self, value: T) {
        self.inner.push(value);
    }

    #[inline]
    pub fn pop(&mut self) -> Option<T> {
        self.inner.pop()
    }

    #[inline]
    pub fn get(&self, idx: usize) -> Option<&T> {
        self.inner.get(idx)
    }

    #[inline]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut T> {
        self.inner.get_mut(idx)
    }

    #[inline]
    pub fn set(&mut self, idx: usize, value: T) -> Result<(), T> {
        if idx >= self.inner.len() {
            return Err(value);
        }
        self.inner[idx] = value;
        Ok(())
    }

    #[inline]
    pub fn iter(&self) -> slice::Iter<'_, T> {
        self.inner.iter()
    }

    #[inline]
    pub fn iter_mut(&mut self) -> slice::IterMut<'_, T> {
        self.inner.iter_mut()
    }
}

impl<T> From<Vec<T>> for MediVec<T> {
    fn from(v: Vec<T>) -> Self {
        Self { inner: v }
    }
}

impl<T> From<MediVec<T>> for Vec<T> {
    fn from(val: MediVec<T>) -> Self {
        val.inner
    }
}

impl<'a, T> IntoIterator for &'a MediVec<T> {
    type Item = &'a T;
    type IntoIter = slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut MediVec<T> {
    type Item = &'a mut T;
    type IntoIter = slice::IterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T> IntoIterator for MediVec<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}
