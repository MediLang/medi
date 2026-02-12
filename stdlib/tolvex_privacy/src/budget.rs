#[derive(Debug, Clone, Copy, PartialEq)]
pub struct PrivacyBudget {
    pub epsilon_total: f64,
    pub delta_total: f64,
    pub epsilon_spent: f64,
    pub delta_spent: f64,
}

impl PrivacyBudget {
    pub fn new(epsilon_total: f64, delta_total: f64) -> Self {
        Self {
            epsilon_total,
            delta_total,
            epsilon_spent: 0.0,
            delta_spent: 0.0,
        }
    }

    pub fn remaining_epsilon(&self) -> f64 {
        (self.epsilon_total - self.epsilon_spent).max(0.0)
    }

    pub fn remaining_delta(&self) -> f64 {
        (self.delta_total - self.delta_spent).max(0.0)
    }

    pub fn can_spend(&self, epsilon: f64, delta: f64) -> bool {
        let eps_ok = self.epsilon_spent + epsilon <= self.epsilon_total;
        let del_ok = self.delta_spent + delta <= self.delta_total;
        eps_ok && del_ok
    }

    pub fn spend(&mut self, epsilon: f64, delta: f64) -> bool {
        if !self.can_spend(epsilon, delta) {
            return false;
        }
        self.epsilon_spent += epsilon;
        self.delta_spent += delta;
        true
    }
}
