use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct ModelUpdate {
    pub client_id: String,
    pub weights: Vec<f64>,
    pub num_examples: u64,
}

impl ModelUpdate {
    pub fn new(client_id: impl Into<String>, weights: Vec<f64>, num_examples: u64) -> Self {
        Self {
            client_id: client_id.into(),
            weights,
            num_examples,
        }
    }
}

pub fn fedavg(updates: &[ModelUpdate]) -> Option<Vec<f64>> {
    if updates.is_empty() {
        return None;
    }

    let dim = updates[0].weights.len();
    if dim == 0 {
        return None;
    }

    let mut total_examples = 0u64;
    for u in updates {
        if u.weights.len() != dim {
            return None;
        }
        total_examples = total_examples.saturating_add(u.num_examples);
    }

    if total_examples == 0 {
        return None;
    }

    let mut out = vec![0.0f64; dim];
    for u in updates {
        let w = (u.num_examples as f64) / (total_examples as f64);
        for (i, slot) in out.iter_mut().enumerate() {
            *slot += w * u.weights[i];
        }
    }
    Some(out)
}

#[derive(Debug, Clone, Default)]
pub struct FederatedCoordinator {
    rounds: HashMap<u64, Vec<ModelUpdate>>,
}

impl FederatedCoordinator {
    pub fn new() -> Self {
        Self {
            rounds: HashMap::new(),
        }
    }

    pub fn submit_update(&mut self, round: u64, update: ModelUpdate) {
        self.rounds.entry(round).or_default().push(update);
    }

    pub fn num_updates(&self, round: u64) -> usize {
        self.rounds.get(&round).map(|v| v.len()).unwrap_or(0)
    }

    pub fn aggregate_round(&self, round: u64) -> Option<Vec<f64>> {
        let updates = self.rounds.get(&round)?;
        fedavg(updates)
    }

    pub fn aggregate_round_min_clients(&self, round: u64, min_clients: usize) -> Option<Vec<f64>> {
        let updates = self.rounds.get(&round)?;
        if updates.len() < min_clients {
            return None;
        }
        fedavg(updates)
    }
}
