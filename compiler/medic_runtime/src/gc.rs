use std::any::Any;
use std::collections::{HashMap, HashSet};
use std::marker::PhantomData;
use std::sync::{Arc, Mutex, Weak};

pub type ObjectId = u64;

#[derive(Debug, Clone)]
pub struct GcParams {
    pub nursery_threshold_bytes: usize,
    pub gen_promotion_threshold: usize,
    pub max_pause_ms: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemoryPressure {
    Low,
    High,
}

impl GcParams {
    pub fn analytics() -> Self {
        Self {
            nursery_threshold_bytes: 8 << 20,
            gen_promotion_threshold: 4,
            max_pause_ms: 20,
        }
    }
}

// Incremental/chunked major GC (feature-gated)
#[cfg(feature = "gc-incremental")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
enum IncPhase {
    Idle,
    Mark,
    Sweep,
}

#[cfg(feature = "gc-incremental")]
struct IncState {
    phase: IncPhase,
    mark_stack: Vec<ObjectId>,
    marked: HashSet<ObjectId>,
    sweep_list: Vec<ObjectId>,
    sweep_index: usize,
}

#[cfg(feature = "gc-incremental")]
impl GarbageCollector {
    /// Start an incremental major GC. Initializes internal worklists.
    pub fn incremental_start_major(&mut self) {
        let mark_stack: Vec<ObjectId> = self.roots.iter().copied().collect();
        let marked: HashSet<ObjectId> = HashSet::new();
        let sweep_list: Vec<ObjectId> = Vec::new();
        let sweep_index = 0;
        self._inc_state_replace(Some(IncState {
            phase: IncPhase::Mark,
            mark_stack,
            marked,
            sweep_list,
            sweep_index,
        }));
    }

    /// Perform incremental work for up to approx `ms_budget` milliseconds. Returns true when the major GC is complete.
    #[allow(clippy::needless_return)]
    pub fn incremental_step(&mut self, ms_budget: u64) -> bool {
        let start = std::time::Instant::now();
        // Derive conservative per-step work caps from budget to bound long tails
        let scale: usize = std::env::var("MEDI_GC_STEP_SCALE")
            .ok()
            .and_then(|s| s.parse::<usize>().ok())
            .filter(|&v| v >= 1)
            .unwrap_or(3);
        // Higher scale => more aggressive tightening (smaller caps)
        let base_mark = (ms_budget as usize).saturating_mul(64).max(64);
        let base_sweep = (ms_budget as usize).saturating_mul(128).max(64);
        let max_mark_units: usize = base_mark / scale;
        let max_sweep_units: usize = base_sweep / scale;
        // Get state or consider GC done if none
        let mut st = match self._inc_state_take() {
            Some(s) => s,
            None => return true,
        };
        match st.phase {
            IncPhase::Mark => {
                // Bounded marking from roots with periodic time checks
                let mut work: usize = 0;
                while let Some(id) = st.mark_stack.pop() {
                    if !st.marked.insert(id) {
                        continue;
                    }
                    if let Some(children) = self.edges.get(&id) {
                        for &c in children {
                            if self.objects.contains_key(&c) {
                                st.mark_stack.push(c);
                            }
                        }
                    }
                    work += 1;
                    if work % 32 == 0 {
                        if start.elapsed().as_millis() as u64 >= ms_budget {
                            self._inc_state_replace(Some(st));
                            return false;
                        }
                        if work >= max_mark_units {
                            self._inc_state_replace(Some(st));
                            return false;
                        }
                    }
                }
                // Transition to sweep
                st.phase = IncPhase::Sweep;
                st.sweep_list = self.objects.keys().copied().collect();
                st.sweep_index = 0;
                self._inc_state_replace(Some(st));
                return false;
            }
            IncPhase::Sweep => {
                // Bounded sweep using the prepared list with periodic time checks
                let mut work: usize = 0;
                while st.sweep_index < st.sweep_list.len() {
                    let id = st.sweep_list[st.sweep_index];
                    st.sweep_index += 1;
                    if !st.marked.contains(&id) {
                        if let Some(fin) = self.finalizers.remove(&id).and_then(|f| f) {
                            fin();
                        }
                        self.objects.remove(&id);
                        self.weak.remove(&id);
                        self.collect_counts.remove(&id);
                        self.edges.remove(&id);
                        for (_p, kids) in self.edges.iter_mut() {
                            kids.retain(|&c| c != id);
                        }
                        self.nursery.remove(&id);
                        self.tenured.remove(&id);
                        self.remembered.remove(&id);
                    } else {
                        // Survived: consider promotion if still in nursery
                        if self.nursery.contains(&id) {
                            let cnt = self.collect_counts.entry(id).or_insert(0);
                            *cnt += 1;
                            if *cnt >= self.params.gen_promotion_threshold {
                                self.nursery.remove(&id);
                                self.tenured.insert(id);
                            }
                        }
                    }
                    work += 1;
                    if work % 32 == 0 {
                        if start.elapsed().as_millis() as u64 >= ms_budget {
                            self._inc_state_replace(Some(st));
                            return false;
                        }
                        if work >= max_sweep_units {
                            self._inc_state_replace(Some(st));
                            return false;
                        }
                    }
                }
                // Done
                self._inc_state_replace(None);
                return true;
            }
            IncPhase::Idle => {
                self._inc_state_replace(None);
                return true;
            }
        }
    }

    fn _inc_state_replace(&mut self, state: Option<IncState>) {
        self._inc_state = state;
    }
    fn _inc_state_take(&mut self) -> Option<IncState> {
        self._inc_state.take()
    }

    /// Convenience: run one incremental step using the configured pause budget.
    pub fn incremental_step_with_params(&mut self) -> bool {
        let budget = self.params.max_pause_ms;
        self.incremental_step(budget)
    }
}

impl Default for GcParams {
    fn default() -> Self {
        Self {
            nursery_threshold_bytes: 1 << 20, // 1MB
            gen_promotion_threshold: 2,       // promote after 2 collections
            max_pause_ms: 10,
        }
    }
}

#[derive(Default)]
pub struct GarbageCollector {
    next_id: ObjectId,
    // Strong table of live objects (by GC ownership). For now, a single generation.
    objects: HashMap<ObjectId, Arc<Mutex<dyn Any + Send>>>,
    // Weak table mirrors objects to support weak refs.
    weak: HashMap<ObjectId, Weak<Mutex<dyn Any + Send>>>,
    // Registered finalizers to call on sweep.
    finalizers: HashMap<ObjectId, Option<Box<dyn FnOnce() + Send>>>,
    // Root set managed explicitly by the host/runtime integrations.
    roots: HashSet<ObjectId>,
    // Adjacency list of edges: parent object id -> child object ids
    edges: HashMap<ObjectId, Vec<ObjectId>>,
    // Generational sets
    nursery: HashSet<ObjectId>,
    tenured: HashSet<ObjectId>,
    // Remembered set: nursery children referenced by tenured parents
    remembered: HashSet<ObjectId>,
    // Feature-gated card table MVP: track tenured parents with potential nursery references
    #[cfg(feature = "gc-card-table")]
    dirty_parents: HashSet<ObjectId>,
    // Collection counters to model promotion heuristics (stubbed).
    collect_counts: HashMap<ObjectId, usize>,
    // Approximate allocation size bookkeeping (for threshold-based collection policies).
    object_sizes: HashMap<ObjectId, usize>,
    nursery_bytes: usize,
    params: GcParams,
    // Simple pause metrics
    last_minor_pause_ms: u64,
    last_major_pause_ms: u64,
    minor_collects: u64,
    major_collects: u64,
    #[cfg(feature = "gc-incremental")]
    _inc_state: Option<IncState>,

    // Memory pressure callbacks (best-effort, intended for host/runtime integrations).
    pressure_listeners: HashMap<usize, Box<dyn Fn(MemoryPressure) + Send + Sync>>,
    next_pressure_listener_id: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct GcStats {
    pub last_minor_pause_ms: u64,
    pub last_major_pause_ms: u64,
    pub minor_collects: u64,
    pub major_collects: u64,
    pub nursery_objects: usize,
    pub tenured_objects: usize,
    pub remembered_objects: usize,
    pub total_objects: usize,
}

impl GarbageCollector {
    pub fn new() -> Self {
        Self {
            params: GcParams::default(),
            ..Default::default()
        }
    }

    pub fn with_params(params: GcParams) -> Self {
        Self {
            params,
            ..Default::default()
        }
    }

    pub fn add_memory_pressure_listener(
        &mut self,
        cb: impl Fn(MemoryPressure) + Send + Sync + 'static,
    ) -> usize {
        let id = self.next_pressure_listener_id;
        self.next_pressure_listener_id = self.next_pressure_listener_id.wrapping_add(1);
        self.pressure_listeners.insert(id, Box::new(cb));
        id
    }

    pub fn remove_memory_pressure_listener(&mut self, id: usize) -> bool {
        self.pressure_listeners.remove(&id).is_some()
    }

    fn notify_memory_pressure(&self, level: MemoryPressure) {
        for cb in self.pressure_listeners.values() {
            cb(level);
        }
    }

    pub fn allocate<T: Any + Send + 'static>(&mut self, value: T) -> GcRef<T> {
        let id = self.next_id;
        self.next_id += 1;
        let approx_bytes = std::mem::size_of::<T>().max(1);
        let arc: Arc<Mutex<dyn Any + Send>> = Arc::new(Mutex::new(value));
        self.objects.insert(id, arc.clone());
        self.weak.insert(id, Arc::downgrade(&arc));
        self.collect_counts.insert(id, 0);
        self.object_sizes.insert(id, approx_bytes);
        self.nursery.insert(id);
        self.nursery_bytes = self.nursery_bytes.saturating_add(approx_bytes);

        if self.should_minor_collect() {
            self.notify_memory_pressure(MemoryPressure::High);
        }
        GcRef {
            id,
            _marker: PhantomData,
        }
    }

    pub fn downgrade<T: Any + Send + 'static>(&self, r: &GcRef<T>) -> GcWeak<T> {
        GcWeak {
            id: r.id,
            _marker: PhantomData,
        }
    }

    pub fn register_finalizer<T: Any + Send + 'static>(
        &mut self,
        r: &GcRef<T>,
        f: impl FnOnce() + Send + 'static,
    ) {
        self.finalizers.insert(r.id, Some(Box::new(f)));
    }

    pub fn add_root<T: Any + Send + 'static>(&mut self, r: &GcRef<T>) {
        self.roots.insert(r.id);
    }

    pub fn remove_root<T: Any + Send + 'static>(&mut self, r: &GcRef<T>) {
        self.roots.remove(&r.id);
    }

    pub fn add_root_id(&mut self, id: ObjectId) {
        self.roots.insert(id);
    }

    pub fn remove_root_id(&mut self, id: ObjectId) {
        self.roots.remove(&id);
    }

    /// Register an edge in the object graph from parent -> child.
    pub fn add_edge(&mut self, parent: ObjectId, child: ObjectId) {
        self.edges.entry(parent).or_default().push(child);
        // Remember nursery child pointed to by tenured parent for faster minor collections
        if self.tenured.contains(&parent) && self.nursery.contains(&child) {
            self.remembered.insert(child);
            #[cfg(feature = "gc-card-table")]
            {
                self.mark_card(parent);
            }
        }
    }

    /// Write barrier: record intergenerational reference without mutating logical edges.
    pub fn write_barrier(&mut self, parent: ObjectId, child: ObjectId) {
        if self.tenured.contains(&parent) && self.nursery.contains(&child) {
            self.remembered.insert(child);
            #[cfg(feature = "gc-card-table")]
            {
                self.mark_card(parent);
            }
        }
    }

    #[cfg(feature = "gc-card-table")]
    fn mark_card(&mut self, parent: ObjectId) {
        self.dirty_parents.insert(parent);
    }

    /// Remove a specific edge from parent -> child, if present.
    pub fn remove_edge(&mut self, parent: ObjectId, child: ObjectId) {
        if let Some(v) = self.edges.get_mut(&parent) {
            v.retain(|&c| c != child);
            if v.is_empty() {
                self.edges.remove(&parent);
            }
        }
    }

    pub fn get<T: Any + Send + 'static>(
        &self,
        r: &GcRef<T>,
    ) -> Option<std::sync::MutexGuard<'_, T>> {
        self.objects.get(&r.id).and_then(|arc| {
            // Downcast the Any to T
            let _guard = arc.lock().ok()?;
            // Safety: We only ever put T under this id via allocate<T>.
            // We can't downcast a guard directly, so temporarily leak and re-lock is complex.
            // Instead, store T behind Any and access via try_downcast in a helper struct.
            None
        })
    }

    pub fn try_read<T: Any + Send + 'static>(&self, r: &GcRef<T>) -> Option<Arc<Mutex<T>>> {
        let arc_any = self.objects.get(&r.id)?.clone();
        // Attempt to downcast Arc<Mutex<dyn Any + Send>> to Arc<Mutex<T>> by extracting inner pointer
        // We can't directly downcast trait objects inside Arc, so we rebuild by cloning data when needed.
        // For runtime, prefer using `with<T, R>(...)` API below.
        let _ = arc_any; // unused in this simplified API
        None
    }

    pub fn with<T: Any + Send + 'static, R>(
        &self,
        r: &GcRef<T>,
        f: impl FnOnce(&mut T) -> R,
    ) -> Option<R> {
        let arc_any = self.objects.get(&r.id)?.clone();
        let mut guard = arc_any.lock().ok()?;
        let any_mut = &mut *guard;
        any_mut.downcast_mut::<T>().map(f)
    }

    fn mark_from_roots(&self) -> HashSet<ObjectId> {
        let mut mark: HashSet<ObjectId> = HashSet::new();
        let mut stack: Vec<ObjectId> = self.roots.iter().copied().collect();
        while let Some(id) = stack.pop() {
            if !mark.insert(id) {
                continue;
            }
            if let Some(children) = self.edges.get(&id) {
                for &c in children {
                    // Only consider children that still exist in object table
                    if self.objects.contains_key(&c) {
                        stack.push(c);
                    }
                }
            }
        }
        mark
    }

    /// Major collection: trace all and sweep both generations.
    pub fn collect_garbage(&mut self) {
        let t0 = std::time::Instant::now();
        self.notify_memory_pressure(MemoryPressure::High);
        // Mark phase
        let mark = self.mark_from_roots();

        // Sweep phase: remove unmarked objects
        let all_ids: Vec<ObjectId> = self.objects.keys().copied().collect();
        for id in all_ids {
            if !mark.contains(&id) {
                if let Some(fin) = self.finalizers.remove(&id).and_then(|f| f) {
                    fin();
                }
                if let Some(sz) = self.object_sizes.remove(&id) {
                    if self.nursery.remove(&id) {
                        self.nursery_bytes = self.nursery_bytes.saturating_sub(sz);
                    }
                } else {
                    let _ = self.nursery.remove(&id);
                }
                self.objects.remove(&id);
                self.weak.remove(&id);
                self.collect_counts.remove(&id);
                self.edges.remove(&id);
                // Also remove incoming edges pointing to this id
                for (_p, kids) in self.edges.iter_mut() {
                    kids.retain(|&c| c != id);
                }
                self.tenured.remove(&id);
                self.remembered.remove(&id);
            } else {
                // Survived: consider promotion if still in nursery
                if self.nursery.contains(&id) {
                    let cnt = self.collect_counts.entry(id).or_insert(0);
                    *cnt += 1;
                    if *cnt >= self.params.gen_promotion_threshold {
                        self.nursery.remove(&id);
                        self.tenured.insert(id);
                        if let Some(sz) = self.object_sizes.get(&id).copied() {
                            self.nursery_bytes = self.nursery_bytes.saturating_sub(sz);
                        }
                    }
                }
            }
        }
        self.last_major_pause_ms = t0.elapsed().as_millis() as u64;
        self.major_collects += 1;
        if std::env::var_os("MEDI_GC_LOG").is_some() {
            eprintln!("gc_major_pause_ms={}", self.last_major_pause_ms);
        }
    }

    /// Minor collection: only sweep nursery; tenured ignored even if unmarked.
    pub fn minor_collect(&mut self) {
        let t0 = std::time::Instant::now();
        if self.should_minor_collect() {
            self.notify_memory_pressure(MemoryPressure::High);
        }
        // Minor marking: start from roots and remembered set, but only traverse within nursery
        let mut mark: HashSet<ObjectId> = HashSet::new();
        let mut stack: Vec<ObjectId> = self.roots.iter().copied().collect();
        // Add remembered nursery nodes
        for &rid in &self.remembered {
            if self.objects.contains_key(&rid) {
                stack.push(rid);
            }
        }
        // Feature-gated: also push children of dirty parents (card table) to seed nursery traversal
        #[cfg(feature = "gc-card-table")]
        {
            let parents: Vec<ObjectId> = self.dirty_parents.iter().copied().collect();
            for p in parents {
                if let Some(children) = self.edges.get(&p) {
                    for &c in children {
                        if self.objects.contains_key(&c) && self.nursery.contains(&c) {
                            stack.push(c);
                        }
                    }
                }
            }
        }
        while let Some(id) = stack.pop() {
            // Only track marks for nursery and roots (roots may be non-nursery)
            if !self.nursery.contains(&id) && !self.roots.contains(&id) {
                continue;
            }
            if !mark.insert(id) {
                continue;
            }
            if let Some(children) = self.edges.get(&id) {
                for &c in children {
                    if self.objects.contains_key(&c) && self.nursery.contains(&c) {
                        stack.push(c);
                    }
                }
            }
        }
        let nursery_ids: Vec<ObjectId> = self.nursery.iter().copied().collect();
        for id in nursery_ids {
            if !mark.contains(&id) {
                if let Some(fin) = self.finalizers.remove(&id).and_then(|f| f) {
                    fin();
                }
                if let Some(sz) = self.object_sizes.remove(&id) {
                    self.nursery_bytes = self.nursery_bytes.saturating_sub(sz);
                }
                self.objects.remove(&id);
                self.weak.remove(&id);
                self.collect_counts.remove(&id);
                self.edges.remove(&id);
                self.nursery.remove(&id);
                self.remembered.remove(&id);
                for (_p, kids) in self.edges.iter_mut() {
                    kids.retain(|&c| c != id);
                }
            } else {
                // Survived minor GC: increment count and maybe promote
                let cnt = self.collect_counts.entry(id).or_insert(0);
                *cnt += 1;
                if *cnt >= self.params.gen_promotion_threshold {
                    self.nursery.remove(&id);
                    self.tenured.insert(id);
                    // No longer need a remembered entry for this object
                    self.remembered.remove(&id);
                    if let Some(sz) = self.object_sizes.get(&id).copied() {
                        self.nursery_bytes = self.nursery_bytes.saturating_sub(sz);
                    }
                }
            }
        }
        self.last_minor_pause_ms = t0.elapsed().as_millis() as u64;
        self.minor_collects += 1;
        #[cfg(feature = "gc-card-table")]
        {
            self.dirty_parents.clear();
        }
        if std::env::var_os("MEDI_GC_LOG").is_some() {
            eprintln!("gc_minor_pause_ms={}", self.last_minor_pause_ms);
        }
    }

    pub fn upgrade<T: Any + Send + 'static>(&self, w: &GcWeak<T>) -> Option<GcRef<T>> {
        // If object still present in objects table, it is considered live.
        if self.objects.contains_key(&w.id) {
            Some(GcRef {
                id: w.id,
                _marker: PhantomData,
            })
        } else {
            None
        }
    }

    #[cfg(test)]
    pub(crate) fn is_tenured(&self, id: ObjectId) -> bool {
        self.tenured.contains(&id)
    }
    #[cfg(test)]
    #[allow(dead_code)]
    pub(crate) fn last_minor_pause_ms(&self) -> u64 {
        self.last_minor_pause_ms
    }
    #[cfg(test)]
    pub(crate) fn last_major_pause_ms(&self) -> u64 {
        self.last_major_pause_ms
    }

    /// Snapshot GC stats (cheap copy for benches/telemetry)
    pub fn stats(&self) -> GcStats {
        GcStats {
            last_minor_pause_ms: self.last_minor_pause_ms,
            last_major_pause_ms: self.last_major_pause_ms,
            minor_collects: self.minor_collects,
            major_collects: self.major_collects,
            nursery_objects: self.nursery.len(),
            tenured_objects: self.tenured.len(),
            remembered_objects: self.remembered.len(),
            total_objects: self.objects.len(),
        }
    }

    #[inline]
    pub fn nursery_bytes(&self) -> usize {
        self.nursery_bytes
    }

    #[inline]
    pub fn should_minor_collect(&self) -> bool {
        self.nursery_bytes >= self.params.nursery_threshold_bytes
    }

    #[cfg(feature = "gc-incremental")]
    #[inline]
    pub fn incremental_is_active(&self) -> bool {
        self._inc_state.is_some()
    }

    #[cfg(feature = "gc-incremental")]
    #[inline]
    pub fn maybe_start_incremental_major(&mut self) {
        if self._inc_state.is_none() {
            // Conservative policy: start an incremental major cycle after enough growth.
            // This is intentionally simple: major GC is bounded by step budget.
            let threshold = self.params.nursery_threshold_bytes.saturating_mul(8);
            let total_bytes: usize = self.object_sizes.values().copied().sum();
            if total_bytes >= threshold {
                self.notify_memory_pressure(MemoryPressure::High);
                self.incremental_start_major();
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcRef<T> {
    id: ObjectId,
    _marker: PhantomData<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GcWeak<T> {
    id: ObjectId,
    _marker: PhantomData<T>,
}

impl<T> GcRef<T> {
    pub fn id(&self) -> ObjectId {
        self.id
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;

    #[test]
    fn weak_ref_drops_after_collect() {
        let mut gc = GarbageCollector::new();
        let obj = gc.allocate::<String>("hello".to_string());
        let w = gc.downgrade(&obj);
        // Not a root, so collection should sweep it.
        gc.collect_garbage();
        assert!(gc.upgrade::<String>(&w).is_none());
    }

    #[test]
    fn root_survives_collect() {
        let mut gc = GarbageCollector::new();
        let obj = gc.allocate::<i32>(123);
        gc.add_root(&obj);
        gc.collect_garbage();
        assert!(gc
            .upgrade::<i32>(&GcWeak {
                id: obj.id,
                _marker: PhantomData
            })
            .is_some());
        // Remove root then collect -> gone
        gc.remove_root(&obj);
        gc.collect_garbage();
        assert!(gc
            .upgrade::<i32>(&GcWeak {
                id: obj.id,
                _marker: PhantomData
            })
            .is_none());
    }

    #[test]
    fn finalizer_runs_on_sweep() {
        static DROPS: AtomicUsize = AtomicUsize::new(0);
        let mut gc = GarbageCollector::new();
        let obj = gc.allocate::<()>(());
        gc.register_finalizer(&obj, || {
            DROPS.fetch_add(1, Ordering::SeqCst);
        });
        gc.collect_garbage();
        assert_eq!(DROPS.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn tracing_keeps_reachable_child() {
        let mut gc = GarbageCollector::new();
        let parent = gc.allocate::<i32>(1);
        let child = gc.allocate::<i32>(2);
        gc.add_edge(parent.id(), child.id());
        gc.add_root(&parent);
        gc.collect_garbage();
        let w = gc.downgrade(&child);
        assert!(gc.upgrade(&w).is_some());
    }

    #[test]
    fn minor_collect_promotes_after_threshold() {
        let mut gc = GarbageCollector::with_params(GcParams {
            nursery_threshold_bytes: 1 << 10,
            gen_promotion_threshold: 1,
            max_pause_ms: 10,
        });
        let r = gc.allocate::<i32>(7);
        gc.add_root(&r);
        gc.minor_collect();
        assert!(gc.is_tenured(r.id()));
    }

    #[test]
    fn leak_stress_alloc_root_unroot_cycles() {
        let mut gc = GarbageCollector::with_params(GcParams {
            nursery_threshold_bytes: 1 << 12,
            gen_promotion_threshold: 2,
            max_pause_ms: 50,
        });
        let mut roots = Vec::new();
        for i in 0..1000 {
            let s = format!("s{i}");
            let r = gc.allocate::<String>(s);
            gc.add_root(&r);
            roots.push(r);
            if i % 50 == 0 {
                gc.minor_collect();
            }
        }
        for r in &roots {
            gc.remove_root(r);
        }
        gc.collect_garbage();
        let _ = gc.last_major_pause_ms(); // touched
                                          // After full GC, most should be swept
        assert!(
            gc.objects.len() < 20,
            "leak-stress: too many live objects: {}",
            gc.objects.len()
        );
    }

    #[test]
    fn cycles_are_collected_when_unreachable() {
        let mut gc = GarbageCollector::new();
        // Make a 2-node cycle a<->b with no roots
        let a = gc.allocate::<i32>(10);
        let b = gc.allocate::<i32>(20);
        gc.add_edge(a.id(), b.id());
        gc.add_edge(b.id(), a.id());
        // Neither a nor b are roots; mark set is empty -> both should be swept
        let wa = gc.downgrade(&a);
        let wb = gc.downgrade(&b);
        gc.collect_garbage();
        assert!(gc.upgrade::<i32>(&wa).is_none());
        assert!(gc.upgrade::<i32>(&wb).is_none());
    }

    #[test]
    fn finalizer_runs_for_child_when_parent_edge_exists() {
        static DROPS2: AtomicUsize = AtomicUsize::new(0);
        let mut gc = GarbageCollector::new();
        // parent -> child, not rooted
        let parent = gc.allocate::<i32>(1);
        let child = gc.allocate::<i32>(2);
        gc.add_edge(parent.id(), child.id());
        gc.register_finalizer(&child, || {
            DROPS2.fetch_add(1, Ordering::SeqCst);
        });
        gc.collect_garbage();
        assert_eq!(DROPS2.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn weak_ref_child_lives_with_parent_root_then_dies_after_unroot() {
        let mut gc = GarbageCollector::new();
        let parent = gc.allocate::<i32>(1);
        let child = gc.allocate::<i32>(2);
        gc.add_edge(parent.id(), child.id());
        let w = gc.downgrade(&child);
        // Root parent -> child should be kept by tracing
        gc.add_root(&parent);
        gc.collect_garbage();
        assert!(gc.upgrade::<i32>(&w).is_some());
        // Remove root and collect -> child should be gone
        gc.remove_root(&parent);
        gc.collect_garbage();
        assert!(gc.upgrade::<i32>(&w).is_none());
    }

    #[test]
    fn nursery_threshold_bytes_triggers_should_minor_collect() {
        let mut gc = GarbageCollector::with_params(GcParams {
            nursery_threshold_bytes: 32,
            gen_promotion_threshold: 2,
            max_pause_ms: 10,
        });
        assert!(!gc.should_minor_collect());
        // Allocate enough i64s to cross 32 bytes (size_of::<i64>() == 8)
        for _ in 0..4 {
            let _ = gc.allocate::<i64>(0);
        }
        assert!(gc.nursery_bytes() >= 32);
        assert!(gc.should_minor_collect());
    }

    #[test]
    fn analytics_params_reduce_minor_collect_frequency_under_allocation() {
        fn run(mut gc: GarbageCollector) -> u64 {
            // Allocate enough to exceed the default 1MB nursery threshold multiple times.
            // We also avoid rooting, so minor GCs will reclaim the nursery and reset bytes.
            for _ in 0..600_000 {
                let _ = gc.allocate::<i64>(0);
                if gc.should_minor_collect() {
                    gc.minor_collect();
                }
            }
            gc.stats().minor_collects
        }

        let default_minor = run(GarbageCollector::new());
        let analytics_minor = run(GarbageCollector::with_params(GcParams::analytics()));
        assert!(
            default_minor > 0,
            "default params should trigger at least one minor GC"
        );
        assert!(
             analytics_minor < default_minor,
             "expected analytics preset to reduce minor GCs: default={default_minor} analytics={analytics_minor}"
         );
    }

    #[cfg(feature = "gc-incremental")]
    #[test]
    fn incremental_major_can_start_and_step() {
        let mut gc = GarbageCollector::with_params(GcParams {
            nursery_threshold_bytes: 64,
            gen_promotion_threshold: 2,
            max_pause_ms: 1,
        });
        // Create some objects and a root so mark phase has work
        let r = gc.allocate::<i64>(1);
        gc.add_root(&r);
        for _ in 0..200 {
            let _ = gc.allocate::<i64>(2);
        }
        gc.maybe_start_incremental_major();
        assert!(gc.incremental_is_active());
        // Ensure the cycle can complete under bounded steps.
        let mut done = false;
        for _ in 0..10_000 {
            if gc.incremental_step_with_params() {
                done = true;
                break;
            }
        }
        assert!(
            done,
            "incremental major GC did not complete within step budget"
        );
    }

    #[test]
    fn memory_pressure_listener_fires_and_can_be_removed() {
        let mut gc = GarbageCollector::with_params(GcParams {
            nursery_threshold_bytes: 32,
            gen_promotion_threshold: 2,
            max_pause_ms: 10,
        });

        let hits = Arc::new(AtomicUsize::new(0));
        let hits2 = hits.clone();
        let id = gc.add_memory_pressure_listener(move |lvl| {
            if matches!(lvl, MemoryPressure::High) {
                hits2.fetch_add(1, Ordering::SeqCst);
            }
        });

        // Cross 32 bytes (4 * i64)
        for _ in 0..4 {
            let _ = gc.allocate::<i64>(0);
        }
        assert!(hits.load(Ordering::SeqCst) > 0);

        assert!(gc.remove_memory_pressure_listener(id));
        let before = hits.load(Ordering::SeqCst);
        for _ in 0..8 {
            let _ = gc.allocate::<i64>(0);
        }
        let after = hits.load(Ordering::SeqCst);
        assert_eq!(before, after);
    }
}
