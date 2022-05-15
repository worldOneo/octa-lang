use std::collections::{HashMap, HashSet};

use crate::program::DataType;

pub struct Stack {
  pub stack_size: usize,
  pub offsets: Vec<HashMap<String, usize>>,
  pub data_types: Vec<HashMap<String, DataType>>,
  pub type_defs: Vec<HashMap<String, DataType>>,
  pub dirty_access: HashSet<(usize, usize)>,
}

impl Stack {
  pub fn new() -> Stack {
    Stack {
      stack_size: 0,
      offsets: vec![HashMap::new()],
      data_types: vec![HashMap::new()],
      type_defs: vec![HashMap::new()],
      dirty_access: HashSet::new(),
    }
  }

  pub fn type_by_name(&self, name: &String) -> Option<DataType> {
    // TODO: Capture variable
    for scope in self.data_types.iter().rev() {
      if let Some(ty) = scope.get(name) {
        return Some(ty.clone());
      }
    }
    None
  }

  pub fn typedef_by_name(&self, name: &String) -> Option<DataType> {
    for scope in self.type_defs.iter().rev() {
      if let Some(ty) = scope.get(name) {
        return Some(ty.clone());
      }
    }
    None
  }

  pub fn set_typedef(&mut self, name: String, ty: DataType) {
    self.type_defs.last_mut().unwrap().insert(name, ty);
  }

  pub fn offset_by_name(&self, name: &String) -> Option<usize> {
    for scope in self.offsets.iter().rev() {
      if let Some(offset) = scope.get(name) {
        return Some(*offset);
      }
    }
    None
  }

  pub fn mark_dirty(&mut self, lower_offset: usize, upper_offset: usize) {
    self.dirty_access.insert((lower_offset, upper_offset));
  }

  pub fn increase_stack_size(&mut self) -> usize {
    self.stack_size += 1;
    self.stack_size - 1
  }

  pub fn push_scope(&mut self) {
    self.offsets.push(HashMap::new());
    self.data_types.push(HashMap::new());
  }

  pub fn pop_scope(&mut self) {
    self.stack_size -= self.offsets.pop().unwrap().len();
    self.data_types.pop();
  }

  pub fn add_var(&mut self, name: String) -> usize {
    if let Some(offset) = self.offsets.last().unwrap().get(&name) {
      return *offset;
    }
    let offset = self.increase_stack_size();
    self.offsets.last_mut().unwrap().insert(name, offset);
    offset
  }

  pub fn set_datatype(&mut self, name: String, ty: DataType) {
    self.data_types.last_mut().unwrap().insert(name, ty);
  }
}
