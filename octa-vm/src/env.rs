use crate::value::Value;

pub struct Env {
  pub values: Vec<Value>,
  pub registers: Vec<Value>,
}

impl Env {
  pub fn new() -> Env {
    Env {
      values: Vec::new(),
      registers: vec![Value::Int(0); 256],
    }
  }

  pub fn push(&mut self, value: Value) {
    self.values.push(value);
  }

  pub fn pop(&mut self) -> Option<Value> {
    self.values.pop()
  }
}