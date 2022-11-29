use crate::value::{Closure, Value};
use octa_build::program;
use octa_parse::parser;

use crate::env::Env;

pub struct VM {
  pub program: Vec<program::Statement>,
  pub env: Env,
}

impl VM {
  pub fn new(program: Vec<program::Statement>) -> VM {
    VM {
      program,
      env: Env::new(),
    }
  }

  pub fn run(&mut self) {
    let mut pc = 0;
    while pc < self.program.len() {
      let statement = &self.program[pc];
      println!(
        "{:?} MEM {:?} CPU {:?} {:?} {:?}",
        statement,
        self.env.stack,
        self.env.registers[0],
        self.env.registers[1],
        self.env.registers[128]
      );
      match statement {
        program::Statement::CopyToReg { reg, offset } => {
          self.env.copy_to_reg(*reg as usize, *offset as usize)
        }
        program::Statement::CopyFromReg { reg, offset } => {
          self.env.copy_from_reg(*reg as usize, *offset as usize)
        }
        program::Statement::Push { reg } => self.env.push(*reg as usize),
        program::Statement::Pop { reg } => self.env.pop(*reg as usize),
        program::Statement::NilReg(reg) => self.env.move_into(*reg as usize, Value::Nil),
        program::Statement::Data { data } => todo!(),
        program::Statement::Label { name } => todo!(),
        program::Statement::Jmp { label } => {
          pc = *label;
          continue;
        }
        program::Statement::JmpIf { reg, label } => {
          if let Value::Bool(v) = self.env.reg(*reg as usize) {
            if *v {
              pc = *label;
              continue;
            }
          }
        }
        program::Statement::Call { reg } => {
          if let Some(pos) = self.prepare_call(*reg as usize) {
            self.env.push_frame(pc + 1);
            pc = pos;
          }
          self.env.move_into(program::REG_RETURN as usize, Value::Nil);
          continue;
        }
        program::Statement::Ret => {
          pc = self.env.pop_frame();
          continue;
        }
        program::Statement::Dupe { reg, val } => self
          .env
          .move_into(*reg as usize, self.env.reg(*val as usize).clone()),
        program::Statement::Op { op } => {
          let res = self.op(*op);
          self.env.move_into(program::REG_RETURN as usize, res);
        }
        program::Statement::UnOp { op } => {
          let res = self.un_op(*op);
          self.env.move_into(program::REG_RETURN as usize, res);
        }
        program::Statement::CaptureClosure {
          elements,
          label,
          reg,
        } => {
          let mut closure = Closure {
            captured: vec![],
            location: *label,
          };
          for i in 0..*elements {
            closure.captured.push(self.env.copy_from_stack(i))
          }
          self
            .env
            .move_into(*reg as usize, Value::Fn(Box::new(closure)))
        }
        program::Statement::SetIntReg(v, r) => self.env.move_into(*r as usize, Value::Int(*v)),
        program::Statement::SetFloatReg(v, r) => self.env.move_into(*r as usize, Value::Float(*v)),
        program::Statement::SetBoolReg(v, r) => self.env.move_into(*r as usize, Value::Bool(*v)),
        program::Statement::SetStringReg(v, r) => self
          .env
          .move_into(*r as usize, Value::String(Box::new(v.clone()))),
        program::Statement::MapGet { reg, key, map } => todo!(),
        program::Statement::ArrayGet { reg, index, array } => todo!(),
        program::Statement::MapInit { reg, size } => todo!(),
        program::Statement::ArrayInit { reg, size } => todo!(),
        program::Statement::StructInit { reg, size } => todo!(),
        program::Statement::DuckGet { reg, ptr, index } => todo!(),
        program::Statement::DuckConv {
          reg,
          ptr,
          index,
          from,
          to,
        } => todo!(),
        program::Statement::CallNative { name } => todo!(),
        program::Statement::StackDec { size } => {
          self.env.dec_stack_size(*size);
        }
      }
      pc += 1;
    }
  }

  fn op(&mut self, op: parser::BinOpType) -> Value {
    let op_a = self.env.reg(program::REG_A as usize);
    let op_b = self.env.reg(program::REG_B as usize);
    if let Value::Int(op_a) = op_a {
      if let Value::Int(op_b) = op_b {
        return match op {
          parser::BinOpType::Lor => Value::Nil,
          parser::BinOpType::Land => Value::Nil,
          parser::BinOpType::Add => Value::Int((*op_a) + (*op_b)),
          parser::BinOpType::Sub => Value::Int((*op_a) - (*op_b)),
          parser::BinOpType::Mul => Value::Int((*op_a) * (*op_b)),
          parser::BinOpType::Div => Value::Int((*op_a) / (*op_b)),
          parser::BinOpType::Mod => Value::Int((*op_a) % (*op_b)),
          parser::BinOpType::Eq => Value::Bool((*op_a) == (*op_b)),
          parser::BinOpType::Ne => Value::Bool((*op_a) != (*op_b)),
          parser::BinOpType::Lt => Value::Bool((*op_a) < (*op_b)),
          parser::BinOpType::Gt => Value::Bool((*op_a) > (*op_b)),
          parser::BinOpType::Le => Value::Bool((*op_a) <= (*op_b)),
          parser::BinOpType::Ge => Value::Bool((*op_a) >= (*op_b)),
          parser::BinOpType::And => Value::Int((*op_a) & (*op_b)),
          parser::BinOpType::Or => Value::Int((*op_a) | (*op_b)),
          parser::BinOpType::Xor => Value::Int((*op_a) ^ (*op_b)),
          parser::BinOpType::Shl => Value::Int((*op_a) << (*op_b)),
          parser::BinOpType::Shr => Value::Int((*op_a) >> (*op_b)),
        };
      }
    }

    if let Value::Float(op_a) = op_a {
      if let Value::Float(op_b) = op_b {
        return match op {
          parser::BinOpType::Add => Value::Float((*op_a) + (*op_b)),
          parser::BinOpType::Sub => Value::Float((*op_a) - (*op_b)),
          parser::BinOpType::Mul => Value::Float((*op_a) * (*op_b)),
          parser::BinOpType::Div => Value::Float((*op_a) / (*op_b)),
          parser::BinOpType::Mod => Value::Float((*op_a) % (*op_b)),
          parser::BinOpType::Eq => Value::Bool((*op_a) == (*op_b)),
          parser::BinOpType::Ne => Value::Bool((*op_a) != (*op_b)),
          parser::BinOpType::Lt => Value::Bool((*op_a) < (*op_b)),
          parser::BinOpType::Gt => Value::Bool((*op_a) > (*op_b)),
          parser::BinOpType::Le => Value::Bool((*op_a) <= (*op_b)),
          parser::BinOpType::Ge => Value::Bool((*op_a) >= (*op_b)),
          _ => Value::Nil,
        };
      }
    }

    if let Value::Bool(op_a) = op_a {
      if let Value::Bool(op_b) = op_b {
        return match op {
          parser::BinOpType::Lor => Value::Bool((*op_a) || (*op_b)),
          parser::BinOpType::Land => Value::Bool((*op_a) && (*op_b)),
          parser::BinOpType::Eq => Value::Bool((*op_a) == (*op_b)),
          parser::BinOpType::Ne => Value::Bool((*op_a) != (*op_b)),
          _ => Value::Nil,
        };
      }
    }

    match op {
      parser::BinOpType::Eq => Value::Bool(false),
      parser::BinOpType::Ne => Value::Bool(true),
      _ => Value::Nil,
    }
  }

  fn un_op(&self, op: parser::UnOpType) -> Value {
    let op_a = self.env.reg(program::REG_A as usize);
    if let Value::Bool(op_a) = op_a {
      match op {
        parser::UnOpType::Neg => Value::Bool(!*op_a),
        parser::UnOpType::Not => Value::Bool(!*op_a),
      }
    } else if let Value::Int(op_a) = op_a {
      match op {
        parser::UnOpType::Neg => Value::Int(-*op_a),
        parser::UnOpType::Not => Value::Int(!*op_a),
      }
    } else if let Value::Float(op_a) = op_a {
      match op {
        parser::UnOpType::Neg => Value::Float(-*op_a),
        parser::UnOpType::Not => Value::Nil,
      }
    } else {
      Value::Nil
    }
  }

  fn prepare_call(&mut self, reg: usize) -> Option<usize> {
    if let Value::Fn(_fn) = self.env.reg(reg).clone() {
      for ele in _fn.captured {
        self.env.push_value(ele);
      }
      Some(_fn.location)
    } else {
      None
    }
  }
}

#[cfg(test)]
mod tests {
  use octa_build::program;
use octa_parse::parser;

use super::program::Statement::*;
  use super::VM;

  #[test]
  fn test1() {
    VM::new(vec![
      CaptureClosure {
        elements: 0,
        label: 3,
        reg: 0,
      },
      Call { reg: 0 },
      Jmp { label: 18 },
      SetIntReg(1, 128),
      CopyFromReg {
        reg: 128,
        offset: 0,
      },
      Jmp { label: 11 },
      CopyFromReg { reg: 0, offset: 0 },
      CopyToReg {
        reg: 128,
        offset: 1,
      },
      Ret,
      NilReg(128),
      Ret,
      CopyToReg { reg: 0, offset: 0 },
      Push { reg: 0 },
      CaptureClosure {
        elements: 1,
        label: 6,
        reg: 128,
      },
      StackDec { size: 1 },
      CopyFromReg {
        reg: 128,
        offset: 1,
      },
      NilReg(128),
      Ret,
    ])
    .run();
  }

  #[test]
  fn test2() {
    VM::new(vec![
      CaptureClosure {
        elements: 0,
        label: 9,
        reg: 0,
      },
      Push { reg: 0 },
      SetIntReg(1, 0),
      Push { reg: 0 },
      SetIntReg(2, 1),
      Pop { reg: 0 },
      Pop { reg: 2 },
      Call { reg: 2 },
      Jmp { label: 27 },
      Push { reg: 0 },
      Push { reg: 1 },
      CopyToReg { reg: 0, offset: 0 },
      Push { reg: 0 },
      CopyToReg { reg: 1, offset: 1 },
      Pop { reg: 0 },
      Op { op: parser::BinOpType::Add },
      CopyFromReg {
        reg: 128,
        offset: 2,
      },
      CopyToReg { reg: 0, offset: 2 },
      Push { reg: 0 },
      SetIntReg(1, 1),
      Pop { reg: 0 },
      Op { op: parser::BinOpType::Add },
      CopyFromReg {
        reg: 128,
        offset: 3,
      },
      CopyToReg {
        reg: 128,
        offset: 3,
      },
      CopyFromReg {
        reg: 128,
        offset: 2,
      },
      NilReg(128),
      Ret,
    ])
    .run();
  }
}
