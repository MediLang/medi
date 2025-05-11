// Type environment for Medic language in Rust
// Mimics TypeScript's TypeEnv for variable/function scopes

use medic_type::types::MediType;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct TypeEnv {
    parent: Option<Box<TypeEnv>>,
    symbols: HashMap<String, MediType>,
}

impl Default for TypeEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv {
            parent: None,
            symbols: HashMap::new(),
        }
    }

    pub fn with_parent(parent: TypeEnv) -> Self {
        TypeEnv {
            parent: Some(Box::new(parent)),
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: String, typ: MediType) {
        self.symbols.insert(name, typ);
    }

    pub fn get(&self, name: &str) -> Option<&MediType> {
        match self.symbols.get(name) {
            Some(typ) => Some(typ),
            None => self.parent.as_ref().and_then(|p| p.get(name)),
        }
    }
}
