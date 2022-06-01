use crate::{map::Map, alloc::GC};

pub struct DuckTable {
  pub offsets: Vec<usize>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ref {
  pub value: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Array {
  pub values: Vec<Value>,
}


pub const HASH_ITER_PRIME: u64 = 31;

impl Array {
  pub fn new() -> Array {
    Array { values: Vec::new() }
  }

  pub fn hash(&self, gc: &GC) -> u64 {
    let mut hash = 0;
    for value in self.values.iter() {
      hash = hash ^ value.hash(gc);
      hash = hash.wrapping_mul(HASH_ITER_PRIME);
    }
    hash
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
  Int(i64),
  Bool(bool),
  Float(f64),
  String(Box<String>),
  Ref(Box<Ref>),
  Array(Box<Array>),
  Map(Box<Map>),
}

const FNV64A_BASIS: u64 = 14695981039346656037;
const FNV64A_PRIME: u64 = 1099511628211;

fn fnv64(value: &[u8]) -> u64 {
  let mut hash = FNV64A_BASIS;
  for byte in value {
    hash = hash ^ (*byte as u64);
    hash = hash.wrapping_mul(FNV64A_PRIME);
  }
  hash
}

impl Value {
  pub fn hash(&self, gc: &GC) -> u64 {
    match self {
      Value::Int(i) => u64::from_le_bytes(i.to_le_bytes()),
      Value::Bool(b) => {
        if *b {
          1
        } else {
          0
        }
      }
      Value::Float(f) => u64::from_le_bytes(f.to_le_bytes()),
      Value::String(s) => fnv64(s.as_bytes()),
      Value::Ref(r) => gc.get_value(r.value.clone()).hash(gc),
      Value::Array(a) => a.hash(gc),
      Value::Map(m) => m.hash(gc),
    }
  }
}
