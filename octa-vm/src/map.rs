use crate::{
  alloc::{Visitor, GC},
  value::{Value, HASH_ITER_PRIME},
};

const KEY_MOVE: usize = 2;
const MAP_SIZE: usize = 1 << 2;

#[derive(Debug, Clone, PartialEq)]
pub enum MapValue {
  Empty,
  Value((Value, Value)),
  Map(Box<Map>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
  pub values: [MapValue; MAP_SIZE],
}

impl Map {
  pub fn visit(&self, visitor: &mut Visitor, gc: &GC) {
    for value in self.values.iter() {
      match value {
        MapValue::Empty => {}
        MapValue::Value((_, value)) => visitor.visit(value, gc),
        MapValue::Map(map) => map.visit(visitor, gc),
      }
    }
  }

  pub fn new() -> Map {
    Map {
      values: [
        MapValue::Empty,
        MapValue::Empty,
        MapValue::Empty,
        MapValue::Empty,
      ],
    }
  }

  pub fn hash(&self, gc: &GC) -> u64 {
    let mut hash = 0;
    for val in self.values.iter() {
      match val {
        MapValue::Empty => {}
        MapValue::Value((key, value)) => {
          hash = hash ^ key.hash(gc);
          hash = hash.wrapping_mul(HASH_ITER_PRIME);
          hash = hash ^ value.hash(gc);
        }
        MapValue::Map(map) => {
          hash = hash ^ map.hash(gc);
          hash = hash.wrapping_mul(HASH_ITER_PRIME);
        }
      }
      hash = hash.wrapping_mul(HASH_ITER_PRIME);
    }
    hash
  }

  pub fn get(&self, index: usize, value: &Value) -> Option<Value> {
    match &self.values[index] {
      MapValue::Empty => None,
      MapValue::Value((k, v)) => {
        if k == value {
          Some(v.clone())
        } else {
          None
        }
      }
      MapValue::Map(m) => m.get(index >> KEY_MOVE, value),
    }
  }
}
