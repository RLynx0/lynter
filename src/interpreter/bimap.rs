#![allow(dead_code)]

use std::{
    collections::{hash_map, HashMap},
    hash::Hash,
};

pub struct BiMap<K, V> {
    primary: HashMap<K, V>,
    secondary: HashMap<V, Vec<K>>,
}

impl<K, V> BiMap<K, V> {
    pub fn new() -> Self {
        BiMap {
            primary: HashMap::new(),
            secondary: HashMap::new(),
        }
    }

    pub fn iter(&self) -> hash_map::Iter<K, V> {
        self.primary.iter()
    }
}

impl<K, V> BiMap<K, V>
where
    K: Eq + Hash + Clone,
    V: Eq + Hash + Clone,
{
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        let prior = self.primary.insert(key.clone(), value.clone());

        if let Some(val) = &prior {
            let keys = self.secondary.get_mut(&val).unwrap();
            *keys = keys
                .into_iter()
                .filter_map(|k| if k == &key { Some(k.to_owned()) } else { None })
                .collect();
        }

        match self.secondary.get_mut(&value) {
            Some(keys) => keys.push(key),
            None => _ = self.secondary.insert(value, vec![key]),
        }

        prior
    }
}

impl<K, V> BiMap<K, V>
where
    K: Eq + Hash,
{
    pub fn get_value<'a>(&'a self, key: &K) -> Option<&'a V> {
        self.primary.get(key)
    }
    pub fn get_value_mut<'a>(&'a mut self, key: &K) -> Option<&'a mut V> {
        self.primary.get_mut(key)
    }
}

impl<K, V> BiMap<K, V>
where
    V: Eq + Hash,
{
    pub fn get_keys<'a>(&'a self, value: &V) -> Option<&'a Vec<K>> {
        self.secondary.get(value)
    }
    pub fn get_keys_mut<'a>(&'a mut self, value: &V) -> Option<&'a mut Vec<K>> {
        self.secondary.get_mut(value)
    }
}
