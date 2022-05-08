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

impl Array {
  pub fn new() -> Array {
    Array { values: Vec::new() }
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
}
