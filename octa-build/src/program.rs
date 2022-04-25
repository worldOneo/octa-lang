use std::{collections::HashMap, vec};

use octa_lex::lexer;
use octa_parse::parser::{self, AssignType, BinOpType, UnOpType, AST};

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub ty: DataType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
  pub name: String,
  pub fields: HashMap<String, StructField>,
  pub generic_types: Vec<String>,
  pub is_generic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
  None,
  Int,
  Float,
  Bool,
  String,
  Array(Box<DataType>),
  Map(Box<DataType>, Box<DataType>),
  Struct(StructType),
  Function(Box<FunctionDataType>),
  Generic(String),
  GenericResolved(HashMap<String, DataType>, Box<DataType>),
  Const(Box<DataType>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDataType {
  pub name: String,
  pub args: Vec<DataType>,
  pub ret: DataType,
  pub generic_types: Vec<String>,
  pub is_generic: bool,
}

impl DataType {
  pub fn is_of(&self, other: &DataType) -> bool {
    if self == other {
      return true;
    }
    if let DataType::Const(inner) = self {
      return inner.is_of(other);
    } else if let DataType::Const(other) = other {
      return self.is_of(other);
    }
    if let DataType::Array(inner) = self {
      if let DataType::Array(other_inner) = other {
        return inner.is_of(other_inner);
      }
    }
    if let DataType::Map(inner_key, inner_value) = self {
      if let DataType::Map(other_key, other_value) = other {
        return inner_key.is_of(other_key) && inner_value.is_of(other_value);
      }
    }
    if let DataType::Struct(inner) = self {
      if let DataType::Struct(other_inner) = other {
        return inner.fields.iter().find(|(name, ty)| {
          other_inner.fields.contains_key(*name) && other_inner.fields[*name].ty.is_of(&ty.ty)
        }) != None;
      }
    }
    if let DataType::Function(inner) = self {
      if let DataType::Function(other_inner) = other {
        return inner.args.len() == other_inner.args.len()
          && inner
            .args
            .iter()
            .zip(other_inner.args.iter())
            .all(|(a, b)| a.is_of(&b))
          && inner.ret.is_of(&other_inner.ret);
      }
    }

    return false;
  }

  pub fn is_const(&self) -> bool {
    match self {
      DataType::Const(_) => true,
      _ => false,
    }
  }
}

pub enum Literal {
  Int(i64),
  Float(f64),
  Bool(bool),
  String(String),
  Map(DataType, DataType),
  Array(DataType),
  Struct(StructType),
}

type Register = u8;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Jmp,
  JmpIf,
  Call,
  Ret,
  Push {
    reg: Register,
  },
  Swap,
  Dupe,
  Pop {
    reg: Register,
  },
  Copy,
  Op {
    op: BinOpType,
  },
  UnOp {
    op: UnOpType,
  },
  SetIntReg(i64, Register),
  SetFloatReg(f64, Register),
  SetBoolReg(bool, Register),
  SetStringReg(String, Register),
  DelayedPushRootOffset {
    reg: Register,
    element: String,
    loc: lexer::CodeLocation,
  },
  CopyToReg {
    reg: Register,
    offset: usize,
  },
  CopyFromReg {
    reg: Register,
    offset: usize,
  },
}
const REG_A: Register = 0;
const REG_B: Register = 1;
const REG_C: Register = 2;
const REG_RETURN: Register = 255;
const REG_ESP: Register = REG_RETURN - 1;

pub type Program = Vec<Statement>;

pub struct Interpreter {
  pub syntax: AST,
  pub stack_sizes: Vec<usize>,
  pub stack_offsets: Vec<Vec<HashMap<String, usize>>>,
  pub root_offsets: HashMap<String, usize>,
  pub data_types: Vec<Vec<HashMap<String, DataType>>>,
  pub type_defs: Vec<Vec<HashMap<String, DataType>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuildError {
  LefthandSideExpectedName(lexer::CodeLocation),
  ConstantReassignment(lexer::CodeLocation),
  TokenDoesntExist(lexer::CodeLocation),
  InvalidType(lexer::CodeLocation),
  PatternExpectd(lexer::CodeLocation),
  TypeHasNoMember(lexer::CodeLocation),
  ParameterTypeMismatch(DataType, DataType, lexer::CodeLocation),
  ReturnTypeMismatch(DataType, DataType, lexer::CodeLocation),
  ParameterZeroIntersection(usize, lexer::CodeLocation),
  ParameterLenMismatch(usize, usize, lexer::CodeLocation),
  InvalidCall(lexer::CodeLocation),
  TypeNotInferable(lexer::CodeLocation),
  GenericTypeMismatch(lexer::CodeLocation),
  TopLevelOperationExpected(lexer::CodeLocation),
  TopLevelBlockExpected(lexer::CodeLocation),
}

pub fn as_name(ast: &AST, loc: lexer::CodeLocation) -> Result<String, BuildError> {
  match ast {
    AST::Variable(name, loc) => Ok(name.clone()),
    _ => Err(BuildError::LefthandSideExpectedName(loc)),
  }
}

pub fn args_match(
  args: &Vec<DataType>,
  params: &Vec<DataType>,
  loc: lexer::CodeLocation,
) -> Result<(), BuildError> {
  if args.len() != params.len() {
    return Err(BuildError::ParameterLenMismatch(
      args.len(),
      params.len(),
      loc,
    ));
  }
  for (arg, param) in args.iter().zip(params.iter()) {
    if !arg.is_of(param) {
      return Err(BuildError::ParameterTypeMismatch(
        arg.clone(),
        param.clone(),
        loc,
      ));
    }
  }
  Ok(())
}

impl Interpreter {
  pub fn new(syntax: AST) -> Interpreter {
    let mut interpreter = Interpreter {
      syntax,
      stack_offsets: vec![vec![HashMap::new()]],
      stack_sizes: vec![0],
      data_types: vec![vec![HashMap::new()]],
      root_offsets: HashMap::new(),
      type_defs: vec![vec![HashMap::new()]],
    };
    interpreter.type_defs[0][0].insert("int".to_string(), DataType::Int);
    interpreter.type_defs[0][0].insert("float".to_string(), DataType::Float);
    interpreter.type_defs[0][0].insert("bool".to_string(), DataType::Bool);
    interpreter.type_defs[0][0].insert("string".to_string(), DataType::String);
    interpreter
  }

  pub fn get_visible_scopes(&self) -> &Vec<HashMap<String, DataType>> {
    &self.data_types[self.stack_sizes.len() - 1]
  }

  pub fn get_current_scope(&self) -> &HashMap<String, DataType> {
    let scopes = self.get_visible_scopes();
    &scopes[scopes.len() - 1]
  }

  pub fn get_root_scopes(&self) -> &HashMap<String, DataType> {
    &self.data_types[0][0]
  }

  pub fn get_visible_offsets(&self) -> &Vec<HashMap<String, usize>> {
    &self.stack_offsets[self.stack_sizes.len() - 1]
  }

  pub fn type_by_name(&self, name: &String) -> Option<DataType> {
    for scope in self.get_visible_scopes().iter().rev() {
      if let Some(ty) = scope.get(name) {
        return Some(ty.clone());
      }
    }

    if let Some(ty) = self.get_root_scopes().get(name) {
      return Some(ty.clone());
    }
    None
  }

  pub fn typedef_by_name(&self, name: &String) -> Option<DataType> {
    for scope in self.type_defs.last().unwrap().iter().rev() {
      if let Some(ty) = scope.get(name) {
        return Some(ty.clone());
      }
    }

    if let Some(ty) = self.type_defs[0][0].get(name) {
      return Some(ty.clone());
    }
    None
  }

  pub fn set_typedef(&mut self, name: &String, ty: DataType) {
    self
      .type_defs
      .last_mut()
      .unwrap()
      .last_mut()
      .unwrap()
      .insert(name.clone(), ty);
  }

  pub fn offset_by_name(&self, name: &String) -> Option<usize> {
    for scope in self.get_visible_offsets().iter().rev() {
      if let Some(offset) = scope.get(name) {
        return Some(*offset);
      }
    }

    if let Some(offset) = self.root_offsets.get(name) {
      return Some(*offset);
    }
    None
  }

  pub fn get_member_type(
    &self,
    first: DataType,
    second: &String,
    loc: lexer::CodeLocation,
  ) -> Result<DataType, BuildError> {
    match first {
      DataType::Struct(struct_type) => {
        if let Some(ty) = struct_type.fields.get(second) {
          return Ok(ty.ty.clone());
        }
        Err(BuildError::TypeHasNoMember(loc))
      }
      _ => Err(BuildError::TypeHasNoMember(loc)),
    }
  }

  pub fn resolve_typedef(&mut self, ast: &AST) -> Result<DataType, BuildError> {
    match ast {
      AST::Variable(name, loc) => self
        .typedef_by_name(name)
        .ok_or(BuildError::InvalidType(loc.clone())),
      AST::StructDefinition(name, generics, fields, loc) => {
        let mut type_fields = HashMap::new();
        for field in fields {
          let field_name = field.0.clone();
          let field_type = self.resolve_typedef(&field.1)?;
          type_fields.insert(field_name, StructField { ty: field_type });
        }
        Ok(DataType::Struct(StructType {
          name: name.clone(),
          fields: type_fields,
          generic_types: generics.clone(),
          is_generic: !generics.is_empty(),
        }))
      }
      AST::GenericIdentifier(ty, resolved, loc) => {
        let ty = self.resolve_typedef(ty)?;
        self.resolve_generic_type(&ty, resolved, loc)
      }
      AST::FunctionDefinition(generics, args, ret, loc) => self.build_fn_def(
        "".to_string(),
        generics,
        args,
        ret,
        &AST::Block(vec![], loc.clone()),
        loc.clone(),
      ),
      _ => Err(BuildError::InvalidType(ast.location())),
    }
  }

  pub fn resolve_generic_type(
    &mut self,
    ty: &DataType,
    resolved: &Vec<AST>,
    loc: &lexer::CodeLocation,
  ) -> Result<DataType, BuildError> {
    let mut args_ty = vec![];
    for ty in resolved {
      args_ty.push(self.resolve_typedef(ty)?);
    }
    let mut resolved_ty = HashMap::new();
    match ty.clone() {
      DataType::Function(func) => {
        if func.generic_types.len() != args_ty.len() {
          return Err(BuildError::GenericTypeMismatch(loc.clone()));
        }
        for (i, ty) in func.generic_types.iter().enumerate() {
          resolved_ty.insert(ty.clone(), args_ty[i].clone());
        }
        return Ok(DataType::GenericResolved(resolved_ty, Box::new(ty.clone())));
      }
      DataType::Struct(stru) => {
        if stru.generic_types.len() != args_ty.len() {
          return Err(BuildError::GenericTypeMismatch(loc.clone()));
        }
        for (i, ty) in stru.generic_types.iter().enumerate() {
          resolved_ty.insert(ty.clone(), args_ty[i].clone());
        }
        return Ok(DataType::GenericResolved(resolved_ty, Box::new(ty.clone())));
      }
      _ => return Err(BuildError::GenericTypeMismatch(loc.clone())),
    }
  }

  pub fn resolve_type(&mut self, ast: &AST) -> Result<DataType, BuildError> {
    match ast {
      AST::Variable(name, loc) => self
        .type_by_name(name)
        .ok_or(BuildError::InvalidType(loc.clone())),
      AST::MemberAccess(first, second, loc) => {
        let first_ty = self.resolve_type(first)?;
        self.get_member_type(first_ty, second, loc.clone())
      }
      AST::Call(callee, args, loc) => {
        let callee_ty = self.resolve_type(callee)?;
        if let DataType::Function(func) = callee_ty {
          let mut args_ty = vec![];
          for arg in args {
            args_ty.push(self.resolve_type(arg)?);
          }
          args_match(&func.args, &args_ty, loc.clone())?;
          return Ok(func.ret.clone());
        }
        Err(BuildError::InvalidCall(loc.clone()))
      }
      AST::MapLiteral(kv, loc) => {
        let mut keys = vec![];
        let mut vals = vec![];
        for kv in kv {
          if let AST::Variable(_, _) = &kv.0 {
            keys.push(DataType::String);
          } else {
            keys.push(self.resolve_type(&kv.0)?);
          }
          vals.push(self.resolve_type(&kv.1)?);
        }
        let key_ty = Interpreter::create_lowest_applicable_type(&keys);
        let val_ty = Interpreter::create_lowest_applicable_type(&vals);
        if key_ty == DataType::None || val_ty == DataType::None {
          return Err(BuildError::TypeNotInferable(loc.clone()));
        }
        Ok(DataType::Map(Box::new(key_ty), Box::new(val_ty)))
      }
      AST::ArrayLiteral(vals, loc) => {
        let mut vals_ty = vec![];
        for val in vals {
          vals_ty.push(self.resolve_type(val)?);
        }
        let ty = Interpreter::create_lowest_applicable_type(&vals_ty);
        if ty == DataType::None {
          return Err(BuildError::TypeNotInferable(loc.clone()));
        }
        Ok(DataType::Array(Box::new(ty)))
      }
      AST::BinOp(first, op, second, loc) => {
        let first = self.resolve_type(first)?;
        let second = self.resolve_type(second)?;
        if op.is_comparison() {
          return Ok(DataType::Bool);
        }
        if first.is_of(&second) {
          return Ok(first);
        }
        Err(BuildError::ParameterTypeMismatch(
          first.clone(),
          second.clone(),
          loc.clone(),
        ))
      }
      AST::GenericIdentifier(ty, resolved, loc) => {
        let ty = self.resolve_type(ty)?;
        self.resolve_generic_type(&ty, resolved, loc)
      }
      AST::IntLiteral(_, _) => Ok(DataType::Int),
      AST::FloatLiteral(_, _) => Ok(DataType::Float),
      AST::StringLiteral(_, _) => Ok(DataType::String),
      AST::BoolLiteral(_, _) => Ok(DataType::Bool),
      _ => Err(BuildError::InvalidType(ast.location())),
    }
  }

  pub fn create_type_intersection(first: &DataType, second: &DataType) -> DataType {
    if first == second {
      return first.clone();
    }
    if let DataType::Struct(first_ty) = first {
      if let DataType::Struct(second_ty) = second {
        let mut fields = HashMap::new();
        for (field_name, _) in &first_ty.fields {
          if let Some(field_type) = second_ty.fields.get(field_name) {
            fields.insert(field_name.clone(), field_type.clone());
          }
        }
        if fields.len() == 0 {
          return DataType::None;
        }
        return DataType::Struct(StructType {
          name: first_ty.name.clone(),
          fields,
          generic_types: vec![],
          is_generic: false,
        });
      }
    }
    return DataType::None;
  }

  pub fn create_lowest_applicable_type(types: &Vec<DataType>) -> DataType {
    types.iter().fold(types[0].clone(), |acc, ty| {
      Interpreter::create_type_intersection(&acc, ty)
    })
  }

  pub fn build_root_types(&mut self) -> Result<(), BuildError> {
    let location = self.syntax.location();
    let syntax = &self.syntax.clone();
    match syntax {
      AST::Block(definitions, loc) => {
        for definition in definitions {
          match definition {
            AST::Initialize(AssignType::Type, name, ty, loc) => {
              let ty = self.resolve_typedef(&ty)?;
              let name = as_name(&name, loc.clone())?;
              self.set_typedef(&name, ty);
            }
            AST::Function(name, generic, args, ret, body, loc) => {
              let ty =
                self.build_fn_def(name.clone(), &generic, &args, &ret, &body, loc.clone())?;
              self.set_data_type(name, ty);
            }
            _ => Err(BuildError::TopLevelOperationExpected(definition.location()))?,
          }
        }
        Ok(())
      }
      _ => Err(BuildError::TopLevelOperationExpected(location))?,
    }
  }

  pub fn build(&mut self) -> Result<Program, BuildError> {
    self.build_root_types()?;
    let syntax = &self.syntax.clone();
    let mut program = vec![];
    match syntax {
      AST::Block(definitions, _) => {
        for definition in definitions {
          program.append(&mut self.build_statement(definition)?);
        }
      }
      _ => todo!(),
    }

    for i in 0..program.len() {
      let stmnt = &program[i];
      if let Statement::DelayedPushRootOffset { reg, element, loc } = stmnt {
        if self.root_offsets.contains_key(element) {
          program[i] = Statement::SetIntReg(self.root_offsets[element] as i64, *reg);
        } else {
          return Err(BuildError::InvalidType(loc.clone()));
        }
      }
    }
    return Ok(program);
  }

  fn set_data_offset(&mut self, name: &str, offset: usize) {
    let last = self.stack_offsets.len() - 1;
    let last_scope = &mut self.stack_offsets[last];
    last_scope
      .last_mut()
      .unwrap()
      .insert(name.to_string(), offset);
  }

  fn get_stack_size(&self) -> usize {
    return self.stack_sizes[self.stack_offsets.len() - 1];
  }

  fn inc_stack_size(&mut self) -> usize {
    let last = self.stack_offsets.len() - 1;
    self.stack_sizes[last] += 1;
    self.stack_sizes[last] - 1
  }

  fn pop_stack(&mut self) {
    self.stack_offsets.pop();
    self.stack_sizes.pop();
    self.data_types.pop();
  }

  fn pop_scope(&mut self) {
    let last = self.stack_sizes.len() - 1;
    let scope_vars = self.get_current_scope().len();
    self.stack_sizes[last] -= scope_vars;
    self.stack_offsets.last_mut().unwrap().pop();
    self.data_types.last_mut().unwrap().pop();
  }

  fn add_stack(&mut self) {
    self.stack_offsets.push(vec![HashMap::new()]);
    self.stack_sizes.push(0);
    self.data_types.push(vec![HashMap::new()]);
  }

  fn add_scope(&mut self) {
    self.stack_offsets.last_mut().unwrap().push(HashMap::new());
    self.data_types.last_mut().unwrap().push(HashMap::new());
  }

  fn stack_add(&mut self, name: &str) -> usize {
    let last = self.stack_offsets.len() - 1;
    let last_scope = &self.stack_offsets[last];
    let scope = last_scope.last().unwrap();
    if scope.contains_key(&name.to_string()) {
      return scope[&name.to_string()];
    }
    let offset = self.inc_stack_size();
    self.stack_offsets[last]
      .last_mut()
      .unwrap()
      .insert(name.to_string(), offset);
    offset
  }

  fn set_data_type(&mut self, name: &String, ty: DataType) {
    let last = self.data_types.len() - 1;
    self.data_types[last]
      .last_mut()
      .unwrap()
      .insert(name.clone(), ty);
  }

  fn push_fn_offset(&self, callee: &AST, reg: Register) -> Result<(Program, DataType), BuildError> {
    match callee {
      AST::Variable(name, loc) => {
        if let Some(ty) = self.type_by_name(name) {
          if let DataType::Function(func) = ty {
            let mut program = vec![];
            if let Some(offset) = self.offset_by_name(name) {
              program.push(Statement::SetIntReg(offset as i64, reg));
            } else {
              program.push(Statement::DelayedPushRootOffset {
                reg: REG_A,
                element: name.to_string(),
                loc: loc.clone(),
              });
            }
            program.push(Statement::Push { reg: reg });
            return Ok((program, func.ret));
          }
        }
        Err(BuildError::InvalidType(loc.clone()))
      }
      _ => Err(BuildError::InvalidType(callee.location())),
    }
  }

  fn build_value_to_reg(
    &mut self,
    value: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    let mut program = vec![];
    let mut ty = DataType::None;
    match value {
      AST::IntLiteral(i, _) => {
        ty = DataType::Int;
        program.push(Statement::SetIntReg(*i, reg))
      }
      AST::FloatLiteral(f, _) => {
        ty = DataType::Float;
        program.push(Statement::SetFloatReg(*f, reg))
      }
      AST::StringLiteral(s, _) => {
        ty = DataType::String;
        program.push(Statement::SetStringReg(s.clone(), reg))
      }
      AST::BoolLiteral(b, _) => {
        ty = DataType::Bool;
        program.push(Statement::SetBoolReg(*b, reg));
      }
      AST::Call(callee, args, _) => {
        let mut offset = self.push_fn_offset(&callee, reg)?;
        program.append(&mut offset.0);
        if let DataType::Function(func) = offset.1 {
          ty = func.ret;
        }
      }
      AST::Variable(name, _) => {
        if let Some(vty) = self.type_by_name(name) {
          ty = vty.clone();
          if let Some(offset) = self.offset_by_name(name) {
            program.push(Statement::CopyToReg { offset, reg });
          } else {
            program.push(Statement::DelayedPushRootOffset {
              reg,
              element: name.to_string(),
              loc: value.location(),
            });
            program.push(Statement::Pop { reg });
          }
        } else {
          Err(BuildError::InvalidType(value.location()))?
        }
      }
      AST::BinOp(lhs, op, rhs, _) => {
        let (mut prog, ty) = self.build_bin_op(lhs, op.clone(), rhs, reg)?;
        program.append(&mut prog);
        return Ok((program, ty));
      }
      _ => todo!(),
    };
    return Ok((program, ty));
  }

  fn build_value_to_ret(&mut self, value: &AST) -> Result<(Program, DataType), BuildError> {
    self.build_value_to_reg(value, REG_RETURN)
  }

  fn build_initialize(
    &mut self,
    assign_type: parser::AssignType,
    lhs: &AST,
    rhs: &AST,
  ) -> Result<Program, BuildError> {
    if lhs.is_pattern() {
      return todo!();
    }
    if let AST::Variable(name, _) = lhs {
      let (mut program, ty) = self.build_value_to_reg(rhs, REG_RETURN)?;
      self.set_data_type(
        name,
        if assign_type == parser::AssignType::Let {
          ty
        } else {
          DataType::Const(Box::new(ty))
        },
      );
      let offset = self.stack_add(name);
      program.push(Statement::CopyFromReg {
        reg: REG_RETURN,
        offset,
      });
      return Ok(program);
    }
    return todo!();
  }

  fn build_statement(&mut self, stmt: &AST) -> Result<Program, BuildError> {
    match stmt {
      AST::Initialize(assign_type, lhs, rhs, _) => {
        let program = self.build_initialize(assign_type.clone(), lhs, rhs)?;
        return Ok(program);
      }
      AST::Assign(lhs, rhs, _) => self.build_assign(lhs, rhs),
      AST::Function(..) => self.build_fn(stmt),
      AST::Return(value, _) => {
        let mut program = vec![];
        if let Some(value) = value {
          let (mut ret, _) = self.build_value_to_ret(value)?;
          program.append(&mut ret);
          program.push(Statement::Ret);
        }
        self.pop_stack();
        return Ok(program);
      }
      _ => todo!(),
    }
  }

  fn build_bin_op(
    &mut self,
    lhs: &AST,
    op: BinOpType,
    rhs: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    let mut program = vec![];
    let (mut lhs_program, lhs_ty) = self.build_value_to_reg(lhs, REG_A)?;
    program.append(&mut lhs_program);
    program.push(Statement::Push { reg: REG_A });
    let (mut rhs_program, rhs_ty) = self.build_value_to_reg(rhs, REG_B)?;
    if !rhs_ty.is_of(&lhs_ty) {
      return Err(BuildError::InvalidType(rhs.location()));
    }
    program.append(&mut rhs_program);
    program.push(Statement::Pop { reg: REG_A });
    program.push(Statement::Op { op });
    program.push(Statement::Push { reg: REG_RETURN });
    program.push(Statement::Pop { reg });
    return Ok((program, lhs_ty));
  }

  fn build_body(&mut self, body: &Vec<AST>) -> Result<Program, BuildError> {
    let mut program = vec![];
    for stmt in body {
      program.append(&mut self.build_statement(stmt)?);
    }
    return Ok(program);
  }

  fn build_fn(&mut self, ast: &AST) -> Result<Program, BuildError> {
    let mut program = vec![];
    match ast {
      AST::Function(name, generics, args, _, body, loc) => {
        self.add_stack();
        for gen in generics {
          self.set_data_type(gen, DataType::Generic(gen.clone()));
        }
        for (i, (name, ty)) in args.iter().enumerate() {
          if name.is_pattern() {
            todo!();
          } else if let AST::Variable(id, _) = name {
            let ty = self.resolve_typedef(&ty)?;
            self.set_data_type(id, ty);
            self.stack_add(id);
          } else {
            todo!();
          }
          program.push(Statement::Push { reg: i as u8 });
        }
        if let AST::Block(body, _) = body.as_ref() {
          program.append(&mut self.build_body(body)?)
        } else {
          todo!();
        }
      }
      _ => todo!(),
    };
    return Ok(program);
  }

  pub fn build_assign(&mut self, lhs: &AST, rhs: &AST) -> Result<Program, BuildError> {
    if lhs.is_pattern() {
      todo!()
    }
    let (mut program, ty) = self.build_value_to_reg(rhs, REG_RETURN)?;
    if let AST::Variable(name, _) = lhs {
      if let Some(full) = self.type_by_name(name) {
        if !ty.is_of(&full) {
          return Err(BuildError::InvalidType(lhs.location()));
        } else if full.is_const() {
          return Err(BuildError::ConstantReassignment(lhs.location()));
        }
      }
      program.push(Statement::CopyFromReg {
        reg: REG_RETURN,
        offset: self.offset_by_name(name).unwrap(),
      });
    } else {
      return Err(BuildError::TokenDoesntExist(lhs.location()));
    }
    Ok(program)
  }

  pub fn resolve_pattern_type(&mut self, pattern: &AST) -> Result<DataType, BuildError> {
    // Might be obsolete
    if !pattern.is_pattern() {
      return Err(BuildError::PatternExpectd(pattern.location()));
    }
    match pattern {
      AST::IntLiteral(..) => Ok(DataType::Int),
      AST::FloatLiteral(..) => Ok(DataType::Float),
      AST::BoolLiteral(..) => Ok(DataType::Bool),
      AST::StringLiteral(..) => Ok(DataType::String),
      AST::MapLiteral(..) | AST::ArrayLiteral(..) | AST::StructLiteral(..) => {
        self.resolve_type(&pattern)
      }
      _ => Err(BuildError::PatternExpectd(pattern.location())),
    }
  }

  pub fn resolve_parameter_type(&mut self, lhs: &AST, rhs: &AST) -> Result<DataType, BuildError> {
    if lhs.is_pattern() {
      self.resolve_pattern_type(lhs)
    } else {
      self.resolve_typedef(rhs)
    }
  }

  pub fn build_fn_def(
    &mut self,
    name: String,
    generic: &Vec<String>,
    args: &Vec<(AST, AST)>,
    ret: &AST,
    body: &AST,
    loc: lexer::CodeLocation,
  ) -> Result<DataType, BuildError> {
    let mut parsed_args = Vec::new();
    if !generic.is_empty() {
      self.add_scope(); // add generic resolved scope
      for arg in generic.iter() {
        let arg_type = DataType::Generic(arg.clone());
        self.set_typedef(arg, arg_type);
      }
    }
    for (lhs, rhs) in args {
      parsed_args.push(self.resolve_parameter_type(&lhs, &rhs)?);
    }
    let ret = self.resolve_typedef(ret)?;
    let current = self.type_by_name(&name);
    if let Some(current) = current {
      if let DataType::Function(current) = current {
        if current.args.len() != parsed_args.len() {
          return Err(BuildError::ParameterLenMismatch(
            current.args.len(),
            parsed_args.len(),
            loc.clone(),
          ));
        }
        for (i, arg) in current.args.iter().enumerate() {
          let ty = Interpreter::create_type_intersection(arg, &parsed_args[i]);
          if ty == DataType::None {
            return Err(BuildError::ParameterZeroIntersection(i, loc.clone()));
          }
          parsed_args[i] = ty;
        }
        if ret != current.ret {
          return Err(BuildError::ReturnTypeMismatch(
            current.ret.clone(),
            ret,
            loc.clone(),
          ));
        }
      }
    }
    if !generic.is_empty() {
      self.pop_scope(); // pop generic resolved scope
    }
    Ok(DataType::Function(Box::new(FunctionDataType {
      name,
      args: parsed_args,
      ret,
      is_generic: !generic.is_empty(),
      generic_types: generic.clone(),
    })))
  }
}

#[cfg(test)]
mod tests {
  use octa_parse::parser;

  use super::*;

  #[test]
  #[rustfmt::skip]
  fn test_types() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
     type i = int
     type f = float
     type b = bool
     type s = string
     fn e(a: i, {a: 1}): string{}
    */
    let ast = AST::Block(vec![
      AST::Initialize(AssignType::Type, Box::new(v("a")), Box::new(v("int")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("b")), Box::new(v("bool")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("c")), Box::new(v("float")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("d")), Box::new(v("string")), l()),
      AST::Function("e".to_string(), vec![], vec![(v("a"), v("int")), (
                AST::MapLiteral(vec![(v("a"), AST::IntLiteral(1, l()))], l()), AST::None(l()))],
                Box::new(v("string")), Box::new(AST::Block(vec![], l())), l())
    ], l());

    let mut interpreter = Interpreter::new(ast);
    interpreter.build_root_types().unwrap();
    assert_eq!(interpreter.typedef_by_name(&"a".to_string()), Some(DataType::Int));
    assert_eq!(interpreter.typedef_by_name(&"b".to_string()), Some(DataType::Bool));
    assert_eq!(interpreter.typedef_by_name(&"c".to_string()), Some(DataType::Float));
    assert_eq!(interpreter.typedef_by_name(&"d".to_string()), Some(DataType::String));
    assert_eq!(interpreter.type_by_name(&"e".to_string()), Some(DataType::Function(Box::new(FunctionDataType {
      name: "e".to_string(),
      args: vec![DataType::Int, DataType::Map(Box::new(DataType::String), Box::new(DataType::Int))],
      ret: DataType::String,
      is_generic: false,
      generic_types: vec![],
    }))));
  }

  #[test]
  #[rustfmt::skip]
  fn test_generic_fn() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
     fn e:[T](a: T): T {
     }
    */
    let ast = AST::Block(vec![
      AST::Function("e".to_string(), vec!["T".to_string()], vec![(v("a"), v("T"))],
                Box::new(v("T")), Box::new(AST::Block(vec![], l())), l())
    ], l());

    let mut interpreter = Interpreter::new(ast);
    interpreter.build_root_types().unwrap();
    assert_eq!(interpreter.type_by_name(&"e".to_string()), Some(
      DataType::Function(Box::new(FunctionDataType {
        name: "e".to_string(),
        args: vec![DataType::Generic("T".to_string())],
        ret: DataType::Generic("T".to_string()),
        is_generic: true,
        generic_types: vec!["T".to_string()],
      }
    ))));
  }

  #[test]
  fn test_generic_resolve() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
    type f = fn [T](a: T): T
    type i = f:[int]
    */
    let ast = AST::Block(
      vec![AST::Initialize(
        AssignType::Type,
        Box::new(v("i")),
        Box::new(AST::GenericIdentifier(
          Box::new(AST::FunctionDefinition(
            vec!["T".to_string()],
            vec![(v("a"), v("T"))],
            Box::new(v("T")),
            l(),
          )),
          vec![v("int")],
          l(),
        )),
        l(),
      )],
      l(),
    );

    let mut interpreter = Interpreter::new(ast);
    interpreter.build_root_types().unwrap();
    assert_eq!(
      interpreter.typedef_by_name(&"i".to_string()),
      Some(DataType::GenericResolved(
        [("T".to_string(), DataType::Int)].iter().cloned().collect(),
        Box::new(DataType::Function(Box::new(FunctionDataType {
          name: "".to_string(),
          args: vec![DataType::Generic("T".to_string())],
          ret: DataType::Generic("T".to_string()),
          generic_types: vec!["T".to_string()],
          is_generic: true
        })))
      ))
    );
  }

  #[test]
  #[rustfmt::skip]
  fn test_build_basic_add() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
     fn e(a: int, b: int): int {
       return a + b
     }
    */
    let ast = AST::Block(vec![
      AST::Function("e".to_string(), vec![], vec![(v("a"), v("int")), (v("b"), v("int"))],
                Box::new(v("int")), Box::new(AST::Block(vec![
                  AST::Return(
                    Some(
                      Box::new(AST::BinOp(Box::new(v("a")),BinOpType::Add,Box::new(v("b")),l()))
                    ), l()
                  )
                ], l())), l())
    ], l());

    let mut interpreter = Interpreter::new(ast);
    println!("{:?}", interpreter.build().unwrap()); // Visual check iguess (^_^)
  }
  #[test]
  #[rustfmt::skip]
  fn test_build_init_read() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
     fn e(a: int, b: int) {
      let c = a + b
      let d = c + 1
      c = d
     }
    */
    let ast = AST::Block(vec![
      AST::Function("e".to_string(), vec![], vec![(v("a"), v("int")), (v("b"), v("int"))],
                Box::new(v("int")), Box::new(AST::Block(vec![
                  AST::Initialize(
                    parser::AssignType::Let,
                    Box::new(v("c")),
                      Box::new(AST::BinOp(
                        Box::new(AST::Variable("a".to_string(), l())),
                        BinOpType::Add,
                        Box::new(AST::Variable("b".to_string(), l())),
                        l(),
                      )
                    ),
                    l()
                  ),
                  AST::Initialize(
                    parser::AssignType::Let,
                    Box::new(v("d")),
                      Box::new(AST::BinOp(
                        Box::new(AST::Variable("c".to_string(), l())),
                        BinOpType::Add,
                        Box::new(AST::IntLiteral(1, l())),
                        l(),
                      )
                    ),
                    l()
                  ),
                  AST::Assign(Box::new(v("c")), Box::new(v("d")), l()),
                ],
              l())),
            l())
    ], l());

    let mut interpreter = Interpreter::new(ast);
    println!("{:?}", interpreter.build().unwrap()); // Visual check iguess (^_^)
  }
}
