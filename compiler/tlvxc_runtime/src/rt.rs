//! Real-time memory zones for deterministic allocation in IoT scenarios.
//! Feature-gated behind `rt_zones`.

use core::mem::{align_of, size_of, MaybeUninit};
use core::ptr;
use core::slice;
use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::ops::{Deref, DerefMut};
use std::time::{Duration, Instant};

use crate::error::{MemoryErrorKind, RuntimeError};
use crate::tolvex_runtime_report;

/// A region allocator with a fixed compile-time capacity.
///
/// - Constant-time bump allocation.
/// - No per-object deallocation; use `reset()` to free the entire region.
/// - No locking; intended for single-threaded RT tasks or externally synchronized use.
pub struct RtRegion<const BYTES: usize> {
    buf: UnsafeCell<[u8; BYTES]>,
    offset: UnsafeCell<usize>,
}

// Provide Default for types that define new()
impl<T, const BYTES: usize, const N: usize> Default for RtZone<T, BYTES, N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const BYTES: usize> Default for RtRegion<BYTES> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T, const N: usize> Default for FixedPool<T, N> {
    fn default() -> Self {
        Self::new()
    }
}

/// RAII handle for objects allocated from `FixedPool`, automatically returning
/// the object to the pool on drop.
pub struct PoolBox<'a, T, const N: usize> {
    pool: &'a FixedPool<T, N>,
    ptr: *mut T,
    _marker: PhantomData<&'a mut T>,
}

impl<'a, T, const N: usize> PoolBox<'a, T, N> {
    /// Leak the handle and return a mutable reference bound to the pool lifetime.
    /// The caller becomes responsible for returning it via `unsafe free_ptr`.
    pub fn into_raw(self) -> *mut T {
        let p = self.ptr;
        std::mem::forget(self);
        p
    }
}

impl<'a, T, const N: usize> Deref for PoolBox<'a, T, N> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr }
    }
}

impl<'a, T, const N: usize> DerefMut for PoolBox<'a, T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.ptr }
    }
}

impl<'a, T, const N: usize> Drop for PoolBox<'a, T, N> {
    fn drop(&mut self) {
        unsafe {
            self.pool.free_ptr(self.ptr);
        }
    }
}

impl<T, const N: usize> FixedPool<T, N> {
    /// Allocate and return a `PoolBox` that auto-returns to pool on drop.
    #[inline]
    pub fn alloc_box(&self, value: T) -> Option<PoolBox<'_, T, N>> {
        let r = self.alloc(value)?;
        let ptr = r as *mut T;
        Some(PoolBox {
            pool: self,
            ptr,
            _marker: PhantomData,
        })
    }
}

/// Scoped view of an `RtRegion` that automatically calls `reset()` on drop.
pub struct RtScope<'a, const BYTES: usize> {
    region: &'a RtRegion<BYTES>,
}

impl<'a, const BYTES: usize> RtScope<'a, BYTES> {
    #[inline]
    pub fn alloc<T>(&self, value: T) -> Option<&'a mut T> {
        // Delegates to region; lifetime ties to &self (which is 'a)
        self.region.alloc(value)
    }

    #[inline]
    pub fn alloc_uninit<T>(&self) -> Option<&'a mut MaybeUninit<T>> {
        self.region.alloc_uninit::<T>()
    }

    #[inline]
    pub fn alloc_array_uninit<T>(&self, len: usize) -> Option<&'a mut [MaybeUninit<T>]> {
        self.region.alloc_array_uninit::<T>(len)
    }

    #[inline]
    pub fn alloc_result<T>(&self, value: T) -> Result<&'a mut T, RuntimeError> {
        self.region.alloc_result(value)
    }

    #[inline]
    pub fn alloc_uninit_result<T>(&self) -> Result<&'a mut MaybeUninit<T>, RuntimeError> {
        self.region.alloc_uninit_result::<T>()
    }

    #[inline]
    pub fn alloc_array_uninit_result<T>(
        &self,
        len: usize,
    ) -> Result<&'a mut [MaybeUninit<T>], RuntimeError> {
        self.region.alloc_array_uninit_result::<T>(len)
    }
}

impl<'a, const BYTES: usize> Drop for RtScope<'a, BYTES> {
    fn drop(&mut self) {
        unsafe { self.region.reset() };
    }
}

impl<const BYTES: usize> RtRegion<BYTES> {
    /// Enter a scoped view; all allocations tied to this scope are reclaimed on drop.
    #[inline]
    pub fn scope(&self) -> RtScope<'_, BYTES> {
        RtScope { region: self }
    }
}

/// Unified real-time zone bundling a region and a fixed-size pool.
pub struct RtZone<T, const BYTES: usize, const N: usize> {
    region: RtRegion<BYTES>,
    pool: FixedPool<T, N>,
}

impl<T, const BYTES: usize, const N: usize> RtZone<T, BYTES, N> {
    #[inline]
    pub const fn new() -> Self {
        Self {
            region: RtRegion::new(),
            pool: FixedPool::new(),
        }
    }

    /// Persistent access to the pool (advanced use).
    #[inline]
    pub fn pool(&self) -> &FixedPool<T, N> {
        &self.pool
    }

    /// Persistent access to the region (advanced use).
    #[inline]
    pub fn region(&self) -> &RtRegion<BYTES> {
        &self.region
    }

    /// Enter a scoped sub-zone; region memory is reclaimed when the scope drops.
    #[inline]
    pub fn scope(&self) -> RtZoneScope<'_, T, BYTES, N> {
        RtZoneScope {
            zone: self,
            rts: self.region.scope(),
        }
    }
}

/// Scope guard for `RtZone` that resets the region on drop and offers safe APIs.
pub struct RtZoneScope<'a, T, const BYTES: usize, const N: usize> {
    zone: &'a RtZone<T, BYTES, N>,
    rts: RtScope<'a, BYTES>,
}

impl<'a, T, const BYTES: usize, const N: usize> RtZoneScope<'a, T, BYTES, N> {
    /// Allocate an object from the pool; auto-return on drop of `PoolBox`.
    #[inline]
    pub fn alloc_pool_box(&self, value: T) -> Option<PoolBox<'a, T, N>> {
        self.zone.pool.alloc_box(value)
    }

    /// Allocate transient memory from the region.
    #[inline]
    pub fn alloc_region<TU>(&self, value: TU) -> Option<&'a mut TU> {
        self.rts.alloc(value)
    }
    #[inline]
    pub fn alloc_region_uninit<TU>(&self) -> Option<&'a mut MaybeUninit<TU>> {
        self.rts.alloc_uninit::<TU>()
    }
    #[inline]
    pub fn alloc_region_array_uninit<TU>(&self, len: usize) -> Option<&'a mut [MaybeUninit<TU>]> {
        self.rts.alloc_array_uninit::<TU>(len)
    }
}

impl<const BYTES: usize> RtRegion<BYTES> {
    pub const fn new() -> Self {
        Self {
            buf: UnsafeCell::new([0u8; BYTES]),
            offset: UnsafeCell::new(0),
        }
    }

    #[inline]
    pub fn capacity(&self) -> usize {
        BYTES
    }

    #[inline]
    pub fn used(&self) -> usize {
        // Safety: single-threaded or externally synchronized use.
        unsafe { *self.offset.get() }
    }

    /// Allocate a value of type `T` with proper alignment from the region.
    /// Returns a mutable reference to uninitialized memory for `T` on success.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit<T>(&self) -> Option<&mut MaybeUninit<T>> {
        let align = align_of::<T>();
        let size = size_of::<T>().max(1);
        // Safety: single-threaded or externally synchronized use.
        let off = unsafe { *self.offset.get() };
        let start_ptr = unsafe { (&*self.buf.get()).as_ptr() as usize };
        let aligned = (start_ptr + off + (align - 1)) & !(align - 1);
        let new_off = aligned + size - start_ptr;
        if new_off > BYTES {
            return None;
        }
        // Safety: within buffer bounds by construction.
        let ptr_t = aligned as *mut T as *mut MaybeUninit<T>;
        unsafe {
            *self.offset.get() = new_off;
            Some(&mut *ptr_t)
        }
    }

    /// Result-returning variant that reports overflow via the runtime reporter.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_uninit_result<T>(&self) -> Result<&mut MaybeUninit<T>, RuntimeError> {
        if let Some(p) = self.alloc_uninit::<T>() {
            Ok(p)
        } else {
            let err = RuntimeError::Memory(MemoryErrorKind::RegionOverflow {
                requested: size_of::<T>().max(1),
                capacity: BYTES,
            });
            tolvex_runtime_report!(err, "rt_region.alloc_uninit_overflow");
            Err(err)
        }
    }

    /// Allocate and write `value` into the region.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc<T>(&self, value: T) -> Option<&mut T> {
        let slot = self.alloc_uninit::<T>()?;
        // Safety: just allocated, properly aligned, within region.
        unsafe {
            ptr::write(slot.as_mut_ptr(), value);
            Some(&mut *slot.as_mut_ptr())
        }
    }

    /// Result-returning variant that reports overflow via the runtime reporter.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_result<T>(&self, value: T) -> Result<&mut T, RuntimeError> {
        match self.alloc_uninit_result::<T>() {
            Ok(slot) => unsafe {
                ptr::write(slot.as_mut_ptr(), value);
                Ok(&mut *slot.as_mut_ptr())
            },
            Err(e) => Err(e),
        }
    }

    /// Allocate a contiguous array of `len` elements of type `T`.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_array_uninit<T>(&self, len: usize) -> Option<&mut [MaybeUninit<T>]> {
        let align = align_of::<T>();
        let size = size_of::<T>().max(1) * len;
        // Safety: single-threaded or externally synchronized use.
        let off = unsafe { *self.offset.get() };
        let start_ptr = unsafe { (&*self.buf.get()).as_ptr() as usize };
        let aligned = (start_ptr + off + (align - 1)) & !(align - 1);
        let new_off = aligned + size - start_ptr;
        if new_off > BYTES {
            return None;
        }
        // Safety: within buffer bounds by construction.
        let ptr_t = aligned as *mut MaybeUninit<T>;
        unsafe {
            *self.offset.get() = new_off;
            Some(slice::from_raw_parts_mut(ptr_t, len))
        }
    }

    /// Result-returning variant that reports overflow via the runtime reporter.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc_array_uninit_result<T>(
        &self,
        len: usize,
    ) -> Result<&mut [MaybeUninit<T>], RuntimeError> {
        if let Some(p) = self.alloc_array_uninit::<T>(len) {
            Ok(p)
        } else {
            let bytes = size_of::<T>().max(1) * len;
            let err = RuntimeError::Memory(MemoryErrorKind::RegionOverflow {
                requested: bytes,
                capacity: BYTES,
            });
            tolvex_runtime_report!(err, "rt_region.alloc_array_overflow");
            Err(err)
        }
    }

    /// Reset the region, invalidating all outstanding references.
    ///
    /// # Safety
    /// The caller must ensure no references from this region are used after reset.
    /// Violating this requirement may cause undefined behavior due to dangling references.
    #[inline]
    pub unsafe fn reset(&self) {
        *self.offset.get() = 0;
    }
}

unsafe impl<const BYTES: usize> Send for RtRegion<BYTES> {}
unsafe impl<const BYTES: usize> Sync for RtRegion<BYTES> {}

/// A fixed-size object pool with O(1) allocate/free operations.
/// Uses a LIFO free list backed by compile-time arrays.
pub struct FixedPool<T, const N: usize> {
    storage: UnsafeCell<[MaybeUninit<T>; N]>,
    freelist: UnsafeCell<[usize; N]>,
    top: UnsafeCell<usize>,
    occupied: UnsafeCell<[bool; N]>,
}

impl<T, const N: usize> FixedPool<T, N> {
    pub const fn new() -> Self {
        // Initialize freelist indices [0..N).
        // Note: const fn limitations; top starts at N meaning all free.
        Self {
            storage: UnsafeCell::new(uninit_array()),
            freelist: UnsafeCell::new(init_indices()),
            top: UnsafeCell::new(N),
            occupied: UnsafeCell::new([false; N]),
        }
    }

    /// Returns current number of free slots.
    #[inline]
    pub fn free(&self) -> usize {
        unsafe { *self.top.get() }
    }

    /// Capacity in elements.
    #[inline]
    pub fn capacity(&self) -> usize {
        N
    }

    /// Allocate a slot and write `value`, returning a mutable reference.
    /// Returns None if the pool is exhausted.
    #[inline]
    #[allow(clippy::mut_from_ref)]
    pub fn alloc(&self, value: T) -> Option<&mut T> {
        let top = unsafe { &mut *self.top.get() };
        if *top == 0 {
            let err = RuntimeError::Memory(MemoryErrorKind::PoolExhausted { capacity: N });
            tolvex_runtime_report!(err, "fixed_pool.alloc_exhausted");
            return None;
        }
        // Pop index from freelist.
        *top -= 1;
        let idx = unsafe { (*self.freelist.get())[*top] };
        let slot = unsafe { &mut (*self.storage.get())[idx] };
        // Safety: slot is currently uninitialized/free.
        unsafe {
            ptr::write(slot.as_mut_ptr(), value);
            // Mark occupied for double-free detection
            (*self.occupied.get())[idx] = true;
            Some(&mut *slot.as_mut_ptr())
        }
    }

    /// Free the object by pointer previously returned by `alloc`.
    ///
    /// # Safety
    /// `ptr` must point to an allocation from this pool and not be double-freed.
    #[inline]
    pub unsafe fn free_ptr(&self, ptr_obj: *mut T) {
        let base = (*self.storage.get()).as_ptr() as *const T;
        let idx = ptr_obj.offset_from(base);
        if !(idx >= 0 && (idx as usize) < N) {
            let err = RuntimeError::Memory(MemoryErrorKind::InvalidFree);
            tolvex_runtime_report!(err, "fixed_pool.invalid_free");
            return;
        }
        let idx = idx as usize;
        let top = &mut *self.top.get();
        // Detect double free via occupancy bitmap
        if !(*self.occupied.get())[idx] {
            let err = RuntimeError::Memory(MemoryErrorKind::DoubleFree { index: idx });
            tolvex_runtime_report!(err, "fixed_pool.double_free");
            return;
        }
        (*self.occupied.get())[idx] = false;
        debug_assert!(*top < N); // Not overflowing
        (*self.freelist.get())[*top] = idx;
        *top += 1;
    }
}

unsafe impl<T: Send, const N: usize> Send for FixedPool<T, N> {}
unsafe impl<T: Sync, const N: usize> Sync for FixedPool<T, N> {}

const fn uninit_array<T, const N: usize>() -> [MaybeUninit<T>; N] {
    // Use const block repeat to avoid requiring T: Copy
    [const { MaybeUninit::<T>::uninit() }; N]
}

const fn init_indices<const N: usize>() -> [usize; N] {
    let mut a = [0usize; N];
    let mut i = 0;
    while i < N {
        a[i] = i;
        i += 1;
    }
    a
}

/// Simple timing harness to approximate per-op latency for allocations.
pub fn verify_latency<F>(iterations: usize, mut f: F) -> Duration
where
    F: FnMut(),
{
    let mut worst = Duration::ZERO;
    for _ in 0..iterations {
        let start = Instant::now();
        f();
        let d = start.elapsed();
        if d > worst {
            worst = d;
        }
    }
    worst
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn region_alloc_and_reset() {
        const CAP: usize = 1024;
        let region = RtRegion::<CAP>::new();
        let a = region.alloc(123u32).unwrap();
        assert_eq!(*a, 123);
        let arr = region.alloc_array_uninit::<u16>(10).unwrap();
        assert_eq!(arr.len(), 10);
        assert!(region.used() > 0);
        unsafe {
            region.reset();
        }
        assert_eq!(region.used(), 0);
        assert!(region.alloc::<u64>(99).is_some());
    }

    #[test]
    fn pool_alloc_free() {
        const N: usize = 64;
        let pool: FixedPool<u32, N> = FixedPool::new();
        let mut ptrs: [*mut u32; N] = [core::ptr::null_mut(); N];
        for (i, slot) in ptrs.iter_mut().enumerate() {
            let r = pool.alloc(i as u32).unwrap();
            *slot = r as *mut u32;
        }
        assert!(pool.alloc(999).is_none()); // exhausted
        unsafe {
            for &p in &ptrs {
                pool.free_ptr(p);
            }
        }
        assert!(pool.alloc(1).is_some());
    }

    #[test]
    fn latency_within_budget_region_and_pool() {
        const CAP: usize = 64 * 1024;
        const N: usize = 1024;
        let region = RtRegion::<CAP>::new();
        let pool: FixedPool<u32, N> = FixedPool::new();

        // Measure region alloc latency
        let worst_region = verify_latency(10_000, || {
            let _ = region.alloc(0u32);
            // ignore potential None on overflow; region is large enough for this count
        });

        // Measure pool alloc/free latency
        let worst_pool = verify_latency(10_000, || {
            if let Some(p) = pool.alloc(1u32) {
                let ptr_p = p as *mut u32;
                unsafe { pool.free_ptr(ptr_p) };
            }
        });

        // Non-binding but indicative threshold: 50 microseconds per op.
        // On CI this should be generous enough; RT goal is << 50us.
        let budget = Duration::from_micros(50);
        assert!(
            worst_region <= budget,
            "region max latency: {worst_region:?}"
        );
        assert!(worst_pool <= budget, "pool max latency: {worst_pool:?}");
    }

    #[test]
    fn iot_simulated_workload_latency() {
        // Simulate a sensor processing loop using a fixed pool for messages and a
        // bump region for transient buffers. Ensure each iteration stays within budget.
        #[derive(Clone, Copy)]
        struct Msg {
            sensor_id: u16,
            value: u16,
        }

        const NMSGS: usize = 256;
        const RBUF: usize = 8 * 1024;
        let pool: FixedPool<Msg, NMSGS> = FixedPool::new();
        let region = RtRegion::<RBUF>::new();

        let iterations = 5_000;
        let budget = core::time::Duration::from_micros(50);
        let mut worst = core::time::Duration::ZERO;
        for i in 0..iterations {
            let start = std::time::Instant::now();

            // Acquire a message from pool
            let m = pool.alloc(Msg {
                sensor_id: (i % 1024) as u16,
                value: (i % 4096) as u16,
            });
            if let Some(mref) = m {
                // Create a small transient buffer in region for formatting/processing
                let buf = region.alloc_array_uninit::<u8>(64).unwrap();
                // Emulate some processing and write back
                let acc = (mref.sensor_id as u32) * 31 + (mref.value as u32) * 7;
                let acc_low = (acc & 0xFF) as u8;
                unsafe {
                    // Just touch the buffer to avoid it being optimized completely away
                    let p = buf.as_ptr() as *mut u8;
                    p.write_volatile(acc_low);
                }
                // Return message to pool
                let p = mref as *mut Msg;
                unsafe { pool.free_ptr(p) };
            }

            let d = start.elapsed();
            if d > worst {
                worst = d;
            }

            // Periodically reset region to reclaim transient buffers
            if i % 128 == 0 {
                unsafe {
                    region.reset();
                }
            }
        }
        assert!(worst <= budget, "iot loop worst-case: {worst:?}");
    }
}
