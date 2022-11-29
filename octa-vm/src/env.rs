use std::collections::HashMap;

use crate::{alloc::GC, value::Value};

pub struct StackTriplet {
  pub stack_pointer: usize,
  pub frame_pointer: usize,
  pub return_address: usize,
}

type NativeFunction = Box<dyn Fn(&Env) -> Value>;

pub struct Env {
  managed_heap: GC,
  pub(crate) stack: Vec<Value>,
  pub(crate) registers: Box<[Value]>,
  stack_triplets: Vec<StackTriplet>,
  native: HashMap<String, NativeFunction>,
}

impl Env {
  pub fn new() -> Env {
    Env {
      managed_heap: GC::new(),
      stack: Vec::new(),
      registers: (vec![Value::Int(0); 256]).into_boxed_slice(),
      stack_triplets: vec![StackTriplet {
        frame_pointer: 0,
        return_address: 0,
        stack_pointer: 0,
      }],
      native: HashMap::new(),
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

  pub fn dec_stack_size(&mut self, n: usize) -> usize {
    let size = self.stack_triplets.last().unwrap().stack_pointer;
    self.stack_triplets.last_mut().unwrap().stack_pointer = size - n;
    size - 1
  }

  pub fn copy_to_reg(&mut self, reg: usize, offset: usize) {
    let val = self.stack[self.offset() + offset].clone();
    self.registers[reg] = val;
  }

  pub fn copy_from_reg(&mut self, reg: usize, offset: usize) {
    let sp = self.offset();
    self.ensure_size(sp + offset);
    self.stack[sp + offset] = self.registers[reg].clone();
  }

  pub fn copy_from_stack(&mut self, offset: usize) -> Value {
    self.stack[self.stack.len() - 1 - offset].clone()
  }

  fn ensure_size(&mut self, size: usize) {
    if size >= self.stack.len() {
      self.stack.resize(
        if self.stack.len() == 0 {
          1
        } else {
          self.stack.len() * 2
        },
        Value::Int(0),
      );
    };
  }

  pub fn push(&mut self, reg: usize) {
    let size = self.inc_stack_size();
    self.ensure_size(size);
    self.stack[size] = self.registers[reg].clone();
  }

  pub fn push_value(&mut self, value: Value) {
    let size = self.inc_stack_size();
    self.ensure_size(size);
    self.stack[size] = value;
  }

  pub fn pop(&mut self, reg: usize) {
    let size = self.dec_stack_size(1);
    self.registers[reg] = self.stack[size].clone();
  }

  pub fn move_into(&mut self, reg: usize, value: Value) {
    self.registers[reg] = value;
  }

  pub fn reg(&self, reg: usize) -> &Value {
    return &self.registers[reg];
  }

  pub fn native_call(&mut self, name: &String) -> Value {
    let _fn = &self.native[name];
    _fn.as_ref()(self)
  }

  pub fn push_frame(&mut self, pos: usize) {
    let frame = self.inc_stack_size();
    self.stack_triplets.push(StackTriplet {
      stack_pointer: 0,
      frame_pointer: frame,
      return_address: pos,
    });
  }

  pub fn pop_frame(&mut self) -> usize {
    let ret = self.stack_triplets.pop().unwrap().return_address;
    self.dec_stack_size(1);
    ret
  }
}
