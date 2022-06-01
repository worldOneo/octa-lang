use crate::{alloc::GC, value::Value};

pub struct StackTriplet {
  pub stack_pointer: usize,
  pub frame_pointer: usize,
  pub return_address: usize,
}

pub struct Env {
  managed_heap: GC,
  stack: Vec<Value>,
  registers: Box<[Value]>,
  stack_triplets: Vec<StackTriplet>,
}

impl Env {
  pub fn new() -> Env {
    Env {
      managed_heap: GC::new(),
      stack: Vec::new(),
      registers: (vec![Value::Int(0); 256]).into_boxed_slice(),
      stack_triplets: Vec::new(),
    }
  }

  pub fn offset(&self) -> usize {
    self.stack_triplets.last().unwrap().frame_pointer
  }

  pub fn inc_stack_size(&mut self) -> usize {
    let size = self.stack_triplets.last().unwrap().stack_pointer;
    self.stack_triplets.last_mut().unwrap().stack_pointer = size + 1;
    size
  }

  pub fn dec_stack_size(&mut self) -> usize {
    let size = self.stack_triplets.last().unwrap().stack_pointer;
    self.stack_triplets.last_mut().unwrap().stack_pointer = size - 1;
    size
  }

  pub fn copy_to_reg(&mut self, reg: usize, offset: usize) {
    let val = self.stack[self.offset() + offset].clone();
    self.registers[reg] = val;
  }

  pub fn copy_from_reg(&mut self, reg: usize, offset: usize) {
    let sp = self.offset();
    self.stack[sp + offset] = self.registers[reg].clone();
  }

  pub fn push(&mut self, reg: usize) {
    let size = self.inc_stack_size();
    if size >= self.stack.len() {
      self.stack.resize(self.stack.len() * 2, Value::Int(0));
    }
    self.stack[size] = self.registers[reg].clone();
  }

  pub fn pop(&mut self, reg: usize) {
    let size = self.dec_stack_size();
    self.registers[reg] = self.stack[size].clone();
  }
}
