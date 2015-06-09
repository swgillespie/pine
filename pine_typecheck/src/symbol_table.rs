use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use types::{Types, Substitution, TypeVar};

pub struct SymbolTable<K, V> {
    global_scope: HashMap<K, V>,
    local_scopes: Vec<HashMap<K, V>>
}

impl<K: Eq + Hash, V> SymbolTable<K, V> {
    pub fn new() -> SymbolTable<K, V> {
        SymbolTable {
            global_scope: HashMap::new(),
            local_scopes: vec![]
        }
    }

    pub fn enter_scope(&mut self) {
        self.local_scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn lookup_mut(&mut self, key: &K) -> Option<&mut V> {
        for table in self.local_scopes.iter_mut().rev() {
            match table.get_mut(key) {
                Some(d) => return Some(d),
                None => ()
            }
        }
        self.global_scope.get_mut(key)
    }

    pub fn lookup(&self, key: &K) -> Option<&V> {
        for table in self.local_scopes.iter().rev() {
            match table.get(key) {
                Some(d) => return Some(d),
                None => ()
            }
        }
        self.global_scope.get(key)
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.local_scopes.last_mut()
            .expect("attempted to insert into global scope through \
                     local insert")
            .insert(key, value)
            .expect("attempted to insert key already in local scope");
    }

    pub fn insert_global(&mut self, key: K, value: V) {
        self.global_scope
            .insert(key, value)
            .expect("attempted to insert key already in global scope");
    }

    pub fn contains(&self, key: &K) -> bool {
        self.lookup(key).is_some()
    }
}
