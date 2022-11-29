use std::collections::HashMap;

use crate::program::Statement;

pub struct Optimizer {
  program: Vec<Statement>,
}

impl Optimizer {
  pub fn new(program: Vec<Statement>) -> Optimizer {
    Optimizer { program }
  }

  pub fn optimize(&self) -> Vec<Statement> {
    let mut pc = 0;
    let mut omit = vec![false; self.program.len()];
    let mut jumps = HashMap::new();
    let mut i: usize = 0;
    while pc < self.program.len() {
      let statement = &self.program[pc];
      match statement {
        Statement::Dupe { reg, val } => {
          if reg == val {
            omit[pc] = true;
          }
        }
        Statement::Push { reg } => {
          if let Some(Statement::Pop { reg: regb }) = self.program.get(pc + 1) {
            omit[pc] = *reg == *regb;
            omit[pc + 1] = omit[pc];
          }
        }
        _ => {}
      }
      pc += 1;
    }
    let program: Vec<Statement> = self
      .program
      .iter()
      .enumerate()
      .filter(|(i, _)| !omit[*i])
      .map(|(_, s)| s.clone())
      .collect();
    for inst in program.iter() {
      match inst {
        Statement::Label { name } => {
          jumps.insert(*name, i);
        }
        _ => {
          i += 1;
        }
      };
    }
    let mut lableless = vec![];
    for inst in program.iter() {
      match inst {
        Statement::Jmp { label } => {
          if let Some(i) = jumps.get(label) {
            lableless.push(Statement::Jmp { label: *i });
          }
        }
        Statement::CaptureClosure {
          elements,
          label,
          reg,
        } => {
          if let Some(i) = jumps.get(label) {
            lableless.push(Statement::CaptureClosure {
              elements: *elements,
              label: *i,
              reg: *reg,
            });
          }
        }
        Statement::Label { .. } => {}
        _ => lableless.push(inst.clone()),
      }
    }
    lableless
  }
}

#[cfg(test)]
mod tests {
  use crate::program::Statement;

  #[test]
  fn test_push_pop() {
    let program = vec![Statement::Push { reg: 0 }, Statement::Pop { reg: 0 }];
    let optimizer = super::Optimizer::new(program);
    let program = optimizer.optimize();
    assert_eq!(program, vec![]);
  }

  #[test]
  fn test_labeless() {
    let program = vec![
      Statement::NilReg(0),
      Statement::Label { name: 0 },
      Statement::Jmp { label: 0 },
    ];
    let optimizer = super::Optimizer::new(program);
    let program = optimizer.optimize();
    assert_eq!(
      program,
      vec![Statement::NilReg(0), Statement::Jmp { label: 1 },]
    );
  }
}
