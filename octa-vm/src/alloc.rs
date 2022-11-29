use std::collections::BTreeSet;

use crate::value::{Array, Ref, Value};

struct BlockVal {
  next: usize,
  value: Value,
}

const BLOCK_END: usize = std::usize::MIN;

pub struct Visitor {
  visited: BTreeSet<usize>,
}

impl Visitor {
  pub fn new() -> Visitor {
    Visitor {
      visited: BTreeSet::new(),
    }
  }

  pub fn visit(&mut self, value: &Value, gc: &GC) {
    match value {
      Value::Int(_) | Value::Bool(_) | Value::Float(_) | Value::String(_) | Value::Nil => {}
      Value::Ref(ref ref_) => {
        if self.visited.insert(ref_.value) {
          self.visit(&gc.get_value(ref_.value), gc);
        }
      }
      Value::Array(ref array) => {
        for value in &array.values {
          self.visit(value, gc);
        }
      }
      Value::Map(ref map) => map.visit(self, gc),
      Value::Fn(_fn) => {
        _fn.captured.iter().for_each(|f| self.visit(f, gc));
      }
    }
  }

  pub fn has_visited(&self, ptr: &usize) -> bool {
    self.visited.contains(ptr)
  }
}

struct ValBlock {
  alloc_ptr: usize,
  values: Box<[BlockVal]>,
}

impl ValBlock {
  pub fn new(capacity: usize) -> ValBlock {
    Self::with_capacity(capacity)
  }

  pub fn with_capacity(capacity: usize) -> ValBlock {
    let mut block = Vec::with_capacity(capacity);
    for i in 0..(capacity - 1) {
      block.push(BlockVal {
        next: i + 1,
        value: Value::Int(0),
      });
    }
    block.push(BlockVal {
      next: BLOCK_END,
      value: Value::Int(0),
    });
    ValBlock {
      alloc_ptr: 0,
      values: block.into_boxed_slice(),
    }
  }

  pub fn alloc(&mut self, value: Value) -> usize {
    let ptr = self.alloc_ptr;
    self.alloc_ptr = self.values[ptr].next;
    self.values[ptr].value = value;
    ptr
  }

  pub fn free(&mut self, ptr: usize) {
    self.values[ptr].next = self.alloc_ptr;
    self.alloc_ptr = ptr;
  }
}

pub struct GC {
  allocated: Vec<usize>,
  blocks: ValBlock,
}

impl GC {
  pub fn new() -> GC {
    GC {
      allocated: Vec::new(),
      blocks: ValBlock::new(256),
    }
  }

  fn alloc_reference(&mut self, value: Value) -> Value {
    let ptr = self.blocks.alloc(value);
    self.allocated.push(ptr);
    Value::Ref(Box::new(Ref { value: ptr }))
  }

  pub fn get_value(&self, ptr: usize) -> &Value {
    &self.blocks.values[ptr].value
  }

  fn mark_active_nodes(&mut self) -> Visitor {
    let mut visitor = Visitor::new();
    for ptr in self.allocated.iter() {
      visitor.visit(self.get_value(*ptr), self);
    }
    visitor
  }

  fn sweep_unmarked_nodes(&mut self, visitor: &Visitor) {
    for ptr in self.allocated.iter() {
      if !visitor.has_visited(&ptr) {
        self.blocks.free(*ptr);
      }
    }
  }

  pub fn gc_cycle(&mut self) {
    let visitor = self.mark_active_nodes();
    self.sweep_unmarked_nodes(&visitor);
  }

  pub fn alloc_array(&mut self) -> Value {
    // TODO: list vs array for alloc/mod performance Q?
    self.alloc_reference(Value::Array(Box::new(Array::new())))
  }
}
