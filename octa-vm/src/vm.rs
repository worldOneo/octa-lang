use octa_build::program;

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
      match statement {
        program::Statement::CopyToReg { reg, offset } => {
          self.env.copy_to_reg(*reg as usize, *offset as usize)
        }
        program::Statement::CopyFromReg { reg, offset } => {
          self.env.copy_from_reg(*reg as usize, *offset as usize)
        }
        program::Statement::Push { reg, .. } => {
          self.env.push(*reg as usize);
        }
        _ => {}
      }
      pc += 1;
    }
  }
}
