use std::{collections::HashMap, vec};

use octa_lex::lexer;
use octa_parse::parser::{self, AssignType, BinOpType, UnOpType, AST};

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
  pub ty: DataType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructType {
  pub special: u8,
  pub fields: HashMap<String, (usize, StructField)>,
  pub sorted_fields: Vec<(String, StructField)>,
  pub generic_types: Vec<String>,
  pub is_generic: bool,
}

const STRUCT_SPECIAL_ARR: u8 = 1;
const STRUCT_SPECIAL_MAP: u8 = 2;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDataType {
  pub name: String,
  pub args: Vec<DataType>,
  pub ret: DataType,
  pub generic_types: Vec<String>,
  pub is_generic: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
  pub data_type: FunctionDataType,
  pub ast: AST,
  pub matched_functions: Vec<Function>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FunctionCompleteErr {
  MultipleCompleteFunctions,
  NoCompleteFunction,
}

impl Function {
  pub fn is_complete(&self) -> bool {
    if let AST::Function(_, _, args, ..) = &self.ast {
      return args.iter().map(|(a, _)| a).all(|a| !a.is_pattern());
    }
    panic!("AST is no function");
  }

  pub fn join_function(&self, other: Function) -> Result<Function, FunctionCompleteErr> {
    if self.ast == other.ast && self.data_type == other.data_type {
      return Ok(self.clone());
    }
    if self.is_complete() {
      if other.is_complete() {
        return Err(FunctionCompleteErr::MultipleCompleteFunctions);
      } else {
        let mut matches = self.matched_functions.clone();
        matches.push(other);
        return Ok(Function {
          data_type: self.data_type.clone(),
          ast: self.ast.clone(),
          matched_functions: matches,
        });
      }
    }
    if other.is_complete() {
      let mut matches = self.matched_functions.clone();
      matches.push(self.clone());
      Ok(Function {
        data_type: other.data_type.clone(),
        ast: other.ast.clone(),
        matched_functions: matches,
      })
    } else {
      let mut matches = self.matched_functions.clone();
      matches.push(other.clone());
      Ok(Function {
        data_type: self.data_type.clone(),
        ast: self.ast.clone(),
        matched_functions: matches,
      })
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DataType {
  Unresolved,
  None,
  Int,
  Float,
  Bool,
  String,
  Struct(StructType),
  Function(Box<FunctionDataType>),
  Generic(String),
  GenericResolved(HashMap<String, DataType>, Box<DataType>),
  Const(Box<DataType>),
}

impl DataType {
  pub fn satisfies(&self, other: &DataType) -> bool {
    if self == other {
      return true;
    }
    if let DataType::Const(inner) = self {
      return inner.satisfies(other);
    } else if let DataType::Const(other) = other {
      return self.satisfies(other);
    }
    if let DataType::Struct(inner) = self {
      if let DataType::Struct(other_inner) = other {
        return other_inner.fields.iter().all(|(name, ty)| {
          inner.fields.contains_key(name) && inner.fields[name].1.ty == ty.1.ty
        });
      }
    }
    if let DataType::Function(inner) = self {
      if let DataType::Function(other_inner) = other {
        return inner.args.len() == other_inner.args.len()
          && inner
            .args
            .iter()
            .zip(other_inner.args.iter())
            .all(|(a, b)| a == b)
          && inner.ret == other_inner.ret;
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

  pub fn is_primitive(&self) -> bool {
    match self {
      DataType::Int | DataType::Float | DataType::Bool | DataType::String => true,
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
pub struct ProgramData {
  pub duck_table: Vec<Vec<usize>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
  Data {
    data: Box<ProgramData>,
  },
  Label {
    name: usize,
  },
  Jmp {
    label: usize,
  },
  JmpIf {
    reg: Register,
    label: usize,
  },
  Call {
    label: usize,
  },
  Ret,
  Push {
    reg: Register,
  },
  Swap,
  Dupe {
    reg: Register,
    val: Register,
  },
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
  CopyToReg {
    reg: Register,
    offset: usize,
  },
  CopyFromReg {
    reg: Register,
    offset: usize,
  },
  MapGet {
    reg: Register,
    key: Register,
    map: Register,
  },
  ArrayGet {
    reg: Register,
    index: Register,
    array: Register,
  },
  MapInit {
    reg: Register,
  },
  ArrayInit {
    reg: Register,
    size: usize,
  },
  StructInit {
    reg: Register,
    size: usize,
  },
  DuckGet {
    reg: Register,
    ptr: Register,
    index: usize,
  },
  DuckConv {
    reg: Register,
    ptr: Register,
    index: usize,
  },
  CallNative {
    name: String,
  },
}
const REG_A: Register = 0;
const REG_B: Register = 1;
const REG_C: Register = 2;
const REG_RETURN: Register = 255;

pub type Program = Vec<Statement>;

pub struct Interpreter {
  pub syntax: AST,
  pub duck_table_index: HashMap<Vec<usize>, usize>,
  pub duck_tables: Vec<Vec<usize>>,
  pub label_count: usize,
  pub stack_sizes: Vec<usize>,
  pub stack_functions: Vec<HashMap<String, (Function, usize)>>,
  pub stack_offsets: Vec<Vec<HashMap<String, usize>>>,
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
  FunctionError(FunctionCompleteErr, lexer::CodeLocation),
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
    if !arg.satisfies(param) {
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
      label_count: 0,
      syntax,
      duck_table_index: HashMap::new(),
      duck_tables: Vec::new(),
      stack_functions: vec![HashMap::new()],
      stack_offsets: vec![vec![HashMap::new()]],
      stack_sizes: vec![0],
      data_types: vec![vec![HashMap::new()]],
      type_defs: vec![vec![HashMap::new()]],
    };
    interpreter.type_defs[0][0].insert("int".to_string(), DataType::Int);
    interpreter.type_defs[0][0].insert("float".to_string(), DataType::Float);
    interpreter.type_defs[0][0].insert("bool".to_string(), DataType::Bool);
    interpreter.type_defs[0][0].insert("string".to_string(), DataType::String);
    interpreter.type_defs[0][0].insert(
      "map".to_string(),
      DataType::Struct(StructType {
        special: STRUCT_SPECIAL_MAP,
        fields: HashMap::new(),
        sorted_fields: vec![],
        generic_types: vec!["K".to_string(), "V".to_string()],
        is_generic: true,
      }),
    );
    interpreter.type_defs[0][0].insert(
      "array".to_string(),
      DataType::Struct(StructType {
        special: STRUCT_SPECIAL_ARR,
        fields: HashMap::new(),
        sorted_fields: vec![],
        generic_types: vec!["T".to_string()],
        is_generic: true,
      }),
    );
    interpreter
  }

  fn build_map_type(k: &DataType, v: &DataType) -> DataType {
    DataType::GenericResolved(
      [("K".to_string(), k.clone()), ("V".to_string(), v.clone())]
        .iter()
        .cloned()
        .collect(),
      Box::new(DataType::Struct(StructType {
        special: STRUCT_SPECIAL_MAP,
        fields: HashMap::new(),
        sorted_fields: vec![],
        generic_types: vec!["K".to_string(), "V".to_string()],
        is_generic: true,
      })),
    )
  }

  fn as_map_key_val(map: &DataType) -> Option<(DataType, DataType)> {
    match map {
      DataType::GenericResolved(args, strct) => {
        if let DataType::Struct(ty) = strct.as_ref() {
          if ty.special != STRUCT_SPECIAL_MAP {
            return None;
          }
          let k = args.get("K").unwrap().clone();
          let v = args.get("V").unwrap().clone();
          Some((k, v))
        } else {
          None
        }
      }
      _ => None,
    }
  }

  fn as_array_val(array: &DataType) -> Option<DataType> {
    match array {
      DataType::GenericResolved(args, strct) => {
        if let DataType::Struct(ty) = strct.as_ref() {
          if ty.special != STRUCT_SPECIAL_ARR {
            return None;
          }
          let v = args.get("T").unwrap().clone();
          Some(v)
        } else {
          None
        }
      }
      _ => None,
    }
  }

  fn build_array_type(t: &DataType) -> DataType {
    DataType::GenericResolved(
      [("T".to_string(), t.clone())].iter().cloned().collect(),
      Box::new(DataType::Struct(StructType {
        special: STRUCT_SPECIAL_ARR,
        fields: HashMap::new(),
        sorted_fields: vec![],
        generic_types: vec!["T".to_string()],
        is_generic: true,
      })),
    )
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

  fn create_label(&mut self) -> usize {
    self.label_count += 1;
    self.label_count
  }

  pub fn offset_by_name(&self, name: &String) -> Option<usize> {
    for scope in self.get_visible_offsets().iter().rev() {
      if let Some(offset) = scope.get(name) {
        return Some(*offset);
      }
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
          return Ok(ty.1.ty.clone());
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
      AST::StructDefinition(generics, fields, loc) => {
        let mut type_fields = HashMap::new();
        let mut fields = fields.clone();
        fields.sort_by(|a, b| a.0.cmp(&b.0));
        for (idx, field) in fields.iter().enumerate() {
          let field_name = field.0.clone();
          let field_type = self.resolve_typedef(&field.1)?;
          type_fields.insert(field_name, (idx, StructField { ty: field_type }));
        }

        let mut sorted_fields = type_fields.clone().into_iter().collect::<Vec<_>>();
        sorted_fields.sort_by(|(name, _), (name2, _)| name.cmp(&name2));
        let sorted_fields = sorted_fields
          .into_iter()
          .map(|(name, (_, field))| (name, field))
          .collect::<Vec<_>>();

        Ok(DataType::Struct(StructType {
          special: 0,
          fields: type_fields,
          sorted_fields: sorted_fields,
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
        Ok(Interpreter::build_map_type(&key_ty, &val_ty))
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
        Ok(Interpreter::build_array_type(&ty))
      }
      AST::BinOp(first, op, second, loc) => {
        let first = self.resolve_type(first)?;
        let second = self.resolve_type(second)?;
        if op.is_comparison() {
          return Ok(DataType::Bool);
        }
        if first == second {
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
    if first == &DataType::Unresolved {
      return second.clone();
    }
    if second == &DataType::Unresolved {
      return first.clone();
    }
    if let DataType::Struct(first_ty) = first {
      if let DataType::Struct(second_ty) = second {
        let mut fields = HashMap::new();
        let mut sorted_fields = vec![];
        for (field_name, _) in &first_ty.fields {
          if let Some(field_type) = second_ty.fields.get(field_name) {
            fields.insert(field_name.clone(), field_type.clone());
            sorted_fields.push((field_name.clone(), field_type.1.clone()));
          }
        }
        return DataType::Struct(StructType {
          special: 0,
          fields,
          sorted_fields,
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

  pub fn build(&mut self) -> Result<Program, BuildError> {
    let syntax = &self.syntax.clone();
    let mut program = vec![];
    program.append(&mut self.build_statement(syntax)?);
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

  fn stack_add_function(&mut self, name: &str, other: Function) -> Result<(), BuildError> {
    let last = self.stack_functions.len() - 1;
    if let Some((function, label)) = self.stack_functions[last].remove(name) {
      let loc = other.ast.location();
      match function.join_function(other) {
        Ok(f) => self.stack_functions[last].insert(name.to_string(), (f, label)),
        Err(e) => return Err(BuildError::FunctionError(e, loc)),
      };
    } else {
      let fn_label = self.create_label();
      self.stack_functions[last].insert(name.to_string(), (other, fn_label));
    }
    Ok(())
  }

  fn set_data_type(&mut self, name: &String, ty: DataType) {
    let last = self.data_types.len() - 1;
    self.data_types[last]
      .last_mut()
      .unwrap()
      .insert(name.clone(), ty);
  }

  fn get_fn_label(&self, callee: &AST) -> Result<usize, BuildError> {
    if let AST::Variable(name, _) = callee {
      for stack in self.stack_functions.iter().rev() {
        if let Some((_, label)) = stack.get(name) {
          return Ok(*label);
        }
      }
    }
    return Err(BuildError::InvalidCall(callee.location()));
  }

  fn build_value_to_reg(
    &mut self,
    value: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    match value {
      AST::IntLiteral(i, _) => Ok((vec![Statement::SetIntReg(*i, reg)], DataType::Int)),
      AST::FloatLiteral(f, _) => Ok((vec![Statement::SetFloatReg(*f, reg)], DataType::Float)),
      AST::StringLiteral(s, _) => Ok((
        vec![Statement::SetStringReg(s.clone(), reg)],
        DataType::String,
      )),
      AST::BoolLiteral(b, _) => Ok((vec![Statement::SetBoolReg(*b, reg)], DataType::Bool)),
      AST::Call(callee, args, _) => {
        let (mut prog, ty) = self.build_call(callee, args)?;
        prog.push(Statement::Dupe {
          reg,
          val: REG_RETURN,
        });
        Ok((prog, ty))
      }
      AST::Variable(name, _) => {
        if let Some(vty) = self.type_by_name(name) {
          if let Some(offset) = self.offset_by_name(name) {
            return Ok((vec![Statement::CopyToReg { offset, reg }], vty.clone()));
          } else {
            return Err(BuildError::InvalidType(value.location()));
          }
        } else {
          Err(BuildError::InvalidType(value.location()))?
        }
      }
      AST::StructLiteral(..) => {
        return self.build_struct_literal_to_reg(value, reg);
      }
      AST::BinOp(lhs, op, rhs, _) => self.build_bin_op(lhs, op.clone(), rhs, reg),
      _ => todo!(),
    }
  }

  fn build_value_to_ret(&mut self, value: &AST) -> Result<(Program, DataType), BuildError> {
    self.build_value_to_reg(value, REG_RETURN)
  }

  fn build_call(
    &mut self,
    callee: &AST,
    args: &Vec<AST>,
  ) -> Result<(Program, DataType), BuildError> {
    if let DataType::Function(func) = self.resolve_type(callee)? {
      let mut program = vec![];
      let label = self.get_fn_label(&callee)?;
      for i in 0..args.len() {
        let arg = &args[i];
        let (mut p, t) = self.build_value_to_reg(arg, i as Register)?;
        program.append(&mut p);
        program.append(&mut self.build_convert_type_to_reg(
          i as Register,
          i as Register,
          &t,
          &func.args[i],
          args[i].location(),
        )?);
        program.push(Statement::Push { reg: i as Register });
      }
      for i in 0..args.len() {
        program.push(Statement::Pop {
          reg: (args.len() - i - 1) as Register,
        });
      }
      program.push(Statement::Call { label });
      return Ok((program, func.ret));
    }
    return Err(BuildError::InvalidType(callee.location()));
  }

  fn build_initialize(
    &mut self,
    assign_type: parser::AssignType,
    lhs: &AST,
    rhs: &AST,
  ) -> Result<Program, BuildError> {
    if assign_type == parser::AssignType::Type {
      return Ok(vec![]);
    }
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
    todo!();
  }

  fn build_statement(&mut self, stmt: &AST) -> Result<Program, BuildError> {
    match stmt {
      AST::Initialize(assign_type, lhs, rhs, _) => {
        let program = self.build_initialize(assign_type.clone(), lhs, rhs)?;
        return Ok(program);
      }
      AST::Assign(lhs, rhs, _) => self.build_assign(lhs, rhs),
      AST::Function(..) => Ok(vec![]), // self.build_fn(stmt), pre collected later build
      AST::Return(value, _) => {
        let mut program = vec![];
        if let Some(value) = value {
          let (mut ret, _) = self.build_value_to_ret(value)?;
          program.append(&mut ret);
          program.push(Statement::Ret);
        }
        return Ok(program);
      }
      AST::Block(block, _) => self.build_body(block),
      AST::Call(callee, args, _) => Ok(self.build_call(callee.as_ref(), args)?.0),
      _ => todo!(),
    }
  }

  fn as_map_key(&mut self, ast: &AST) -> AST {
    match ast {
      AST::Variable(name, loc) => AST::StringLiteral(name.clone(), loc.clone()),
      ast => ast.clone(),
    }
  }

  fn build_param_pattern(
    &mut self,
    stmt: &AST,
    val: usize,
    ty: DataType,
    skip: usize,
  ) -> Result<Program, BuildError> {
    match stmt {
      AST::MapLiteral(map, _) => {
        if let Some((key_ty, val_ty)) = Interpreter::as_map_key_val(&ty) {
          let mut program = vec![];
          for (key, value) in map {
            let key = &self.as_map_key(key);
            let (mut key_prog, pat_ty) = self.build_value_to_reg(key, REG_A)?;
            if !pat_ty.satisfies(&key_ty) {
              return Err(BuildError::InvalidType(key.location()));
            }
            program.append(&mut key_prog);
            program.push(Statement::CopyToReg {
              reg: REG_B,
              offset: val,
            });
            program.push(Statement::MapGet {
              reg: REG_RETURN,
              key: REG_A,
              map: REG_B,
            });
            program.push(Statement::Push { reg: REG_RETURN });
            let map_val = self.inc_stack_size();
            let mut value_prog = self.build_param_pattern(value, map_val, val_ty.clone(), skip)?;
            program.append(&mut value_prog);
          }
          Ok(program)
        } else {
          Err(BuildError::InvalidType(stmt.location()))
        }
      }
      AST::ArrayLiteral(array, _) => {
        if let Some(val_ty) = Interpreter::as_array_val(&ty) {
          let mut program = vec![];
          for (i, value) in array.iter().enumerate() {
            program.push(Statement::SetIntReg(i as i64, REG_A));
            program.push(Statement::CopyToReg {
              reg: REG_B,
              offset: val,
            });
            program.push(Statement::ArrayGet {
              reg: REG_RETURN,
              index: REG_A,
              array: REG_B,
            });
            program.push(Statement::Push { reg: REG_RETURN });
            let array_val = self.inc_stack_size();
            let mut value_prog =
              self.build_param_pattern(value, array_val, val_ty.clone(), skip)?;
            program.append(&mut value_prog);
          }
          Ok(program)
        } else {
          Err(BuildError::InvalidType(stmt.location()))
        }
      }
      ast => {
        let mut program = vec![];
        if let AST::Variable(name, loc) = ast {
          let offset = self.stack_add(name);
          self.set_data_type(name, ty);
          program.push(Statement::CopyToReg { reg: REG_B, offset });
          program.push(Statement::Push { reg: REG_B });
          self.inc_stack_size();
          return Ok(program);
        }
        let (mut create, val_ty) = self.build_value_to_reg(ast, REG_A)?;
        program.append(&mut create);
        if !val_ty.satisfies(&ty) {
          return Err(BuildError::InvalidType(ast.location()));
        }
        program.push(Statement::CopyToReg {
          reg: REG_B,
          offset: val,
        });
        program.push(Statement::Op { op: BinOpType::Eq });
        program.push(Statement::JmpIf {
          reg: REG_RETURN,
          label: skip,
        });
        Ok(program)
      }
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
    if !rhs_ty.satisfies(&lhs_ty) {
      return Err(BuildError::InvalidType(rhs.location()));
    }
    program.append(&mut rhs_program);
    program.push(Statement::Pop { reg: REG_A });
    program.push(Statement::Op { op });
    program.push(Statement::Push { reg: REG_RETURN });
    program.push(Statement::Pop { reg });
    return Ok((program, lhs_ty));
  }

  fn collect_referencable_labels(&mut self, ast: &Vec<AST>) -> Result<(), BuildError> {
    for stmt in ast {
      match stmt {
        AST::Initialize(AssignType::Type, name, ty, loc) => {
          let ty = self.resolve_typedef(&ty)?;
          let name = as_name(&name, loc.clone())?;
          self.set_typedef(&name, ty);
        }
        AST::Function(name, generic, args, ret, body, loc) => {
          let ty = self.build_fn_def(name.clone(), &generic, &args, &ret, &body, loc.clone())?;
          if let DataType::Function(_) = &ty {
            let (name, f) = self.build_fn(stmt)?;
            self.set_data_type(&name, ty);
            self.stack_add_function(&name, f)?;
          } else {
            panic!("build_fn_def didn't return a function")
          }
        }
        _ => {}
      }
    }
    Ok(())
  }

  fn build_body(&mut self, body: &Vec<AST>) -> Result<Program, BuildError> {
    self.stack_functions.push(HashMap::new());
    self.collect_referencable_labels(body)?;
    let mut program = vec![];
    for stmt in body {
      program.append(&mut self.build_statement(stmt)?);
    }
    let functions = self.stack_functions.last().unwrap().clone();
    let functions = functions.values();
    for (function, label) in functions {
      program.push(Statement::Label { name: *label });
      program.append(&mut self.build_patterned_function(function)?);
    }
    self.stack_functions.pop();
    return Ok(program);
  }

  fn add_generic_scope(&mut self, generic: &Vec<String>) {
    self.add_scope();
    for arg in generic.iter() {
      let arg_type = DataType::Generic(arg.clone());
      self.set_typedef(arg, arg_type);
    }
  }

  fn build_patterned_function(&mut self, func: &Function) -> Result<Program, BuildError> {
    if !func.is_complete() {
      return Err(BuildError::FunctionError(
        FunctionCompleteErr::NoCompleteFunction,
        func.ast.location(),
      ));
    }
    let mut program = vec![];
    if let AST::Function(name, generics, args, ret, body, loc) = &func.ast {
      self.add_stack();
      self.add_generic_scope(&generics);
      for (i, _) in args.iter().enumerate() {
        program.push(Statement::Push { reg: i as u8 });
      }
      let skip = self.create_label();
      let mut funcs = func.matched_functions.clone();
      funcs.push(func.clone());
      for pattern in &funcs {
        if let AST::Function(_, _, args, ..) = &pattern.ast {
          for (i, (name, _)) in args.iter().enumerate() {
            let ty = func.data_type.args[i].clone();
            if name.is_pattern() {
              let val = self.inc_stack_size();
              let mut pattern_program = self.build_param_pattern(name, val, ty, skip)?;
              program.append(&mut pattern_program);
            } else if let AST::Variable(id, _) = name {
              self.set_data_type(id, ty);
              self.stack_add(id);
            } else {
              todo!();
            }
          }
          if pattern.is_complete() {
            let mut body_program = self.build_statement(body.as_ref())?;
            program.push(Statement::Label { name: skip });
            program.append(&mut body_program);
          }
        } else {
          todo!();
        }
      }
      self.pop_scope();
      self.pop_stack();
    }
    Ok(program)
  }

  fn build_fn(&mut self, ast: &AST) -> Result<(String, Function), BuildError> {
    match ast {
      AST::Function(name, generics, args, ret, body, loc) => {
        let ty = self.build_fn_def(name.to_string(), &generics, &args, &ret, &body, loc.clone())?;
        if let DataType::Function(fn_ty) = ty {
          return Ok((
            name.into(),
            Function {
              data_type: *fn_ty.clone(),
              ast: ast.clone(),
              matched_functions: vec![],
            },
          ));
        } else {
          todo!();
        }
      }
      _ => todo!("build_fn ast is not a function"),
    }
  }

  // Duck typing
  fn add_duck_table(&mut self, table: &Vec<usize>) -> usize {
    if let Some(index) = self.duck_table_index.get(table) {
      return *index;
    }
    let index = self.duck_tables.len();
    self.duck_table_index.insert(table.clone(), index);
    self.duck_tables.push(table.clone());
    return index;
  }

  fn build_satisfaction_table(&mut self, reach: &DataType, duck: &DataType) -> Option<usize> {
    if !duck.satisfies(reach) {
      panic!("build_satisfaction_table: duck does not satisfy reach");
    }
    if let DataType::Struct(strct) = reach {
      if let DataType::Struct(duck_strct) = duck {
        let mut diff = vec![];
        let other_idx: usize = 0;
        let duck_fields = &duck_strct.sorted_fields;
        let reach_fields = &strct.sorted_fields;
        for (idx, field) in duck_fields.iter().enumerate() {
          if let Some((name, duck_ty)) = reach_fields.get(other_idx) {
            if &field.0 == name && (field.1).ty == duck_ty.ty {
              diff.push(idx);
              continue;
            } else if &field.0 == name && (field.1).ty != duck_ty.ty {
              return None;
            }
          }
        }
        return Some(self.add_duck_table(&diff));
      }
    }
    return None;
  }

  fn build_struct_literal_to_reg(
    &mut self,
    ast: &AST,
    reg: u8,
  ) -> Result<(Program, DataType), BuildError> {
    match ast {
      AST::StructLiteral(name, fields, _) => {
        let ty = self.resolve_typedef(name)?;
        if let DataType::Struct(struct_ty) = &ty {
          let mut program = vec![];
          for (_, (name, field)) in fields.iter().enumerate() {
            let (mut field_program, field_ty) = self.build_value_to_reg(field, REG_A as u8)?;
            program.append(&mut field_program);
            program.push(Statement::Push { reg: REG_A });
            if field_ty != struct_ty.fields[name].1.ty {
              return Err(BuildError::InvalidType(field.location()));
            }
          }
          for (i, _) in fields.iter().enumerate() {
            program.push(Statement::Pop {
              reg: (fields.len() - i) as u8,
            });
          }
          program.push(Statement::StructInit {
            reg: REG_RETURN,
            size: fields.len(),
          });
          program.push(Statement::Push { reg });
          return Ok((program, ty.clone()));
        } else {
          return Err(BuildError::InvalidType(ast.location()));
        }
      }
      _ => todo!(),
    };
  }

  fn build_empty_type_to_reg(
    &mut self,
    ty: &DataType,
    reg: Register,
  ) -> Result<Program, BuildError> {
    match &ty {
      DataType::Struct(strct) => {
        let mut program = vec![];
        if let Some(_) = Self::as_array_val(ty) {
          program.push(Statement::ArrayInit { reg, size: 0 });
          return Ok(program);
        } else if let Some((_, _)) = Self::as_map_key_val(ty) {
          program.push(Statement::MapInit { reg });
          return Ok(program);
        }
        for (_, field) in strct.sorted_fields.iter().enumerate() {
          program.append(&mut self.build_empty_type_to_reg(&field.1.ty, REG_A)?);
          program.push(Statement::Push { reg: REG_A });
        }
        for i in 0..strct.fields.len() {
          program.push(Statement::Pop { reg: i as u8 });
        }

        program.push(Statement::StructInit {
          reg: reg,
          size: strct.fields.len(),
        });
        return Ok(program);
      }
      DataType::Bool => {
        return Ok(vec![Statement::SetBoolReg(false, reg)]);
      }
      DataType::Int => {
        return Ok(vec![Statement::SetIntReg(0, reg)]);
      }
      DataType::Float => {
        return Ok(vec![Statement::SetFloatReg(0.0, reg)]);
      }
      DataType::String => {
        return Ok(vec![Statement::SetStringReg("".to_string(), reg)]);
      }
      _ => todo!(),
    }
  }

  fn build_convert_type_to_reg(
    &mut self,
    val: Register,
    reg: Register,
    from: &DataType,
    to: &DataType,
    loc: lexer::CodeLocation,
  ) -> Result<Program, BuildError> {
    if !from.satisfies(to) {
      return Err(BuildError::InvalidType(loc));
    }
    if from.is_primitive() || from == to {
      return Ok(vec![Statement::Dupe { val, reg }]);
    }
    if let Some(table) = self.build_satisfaction_table(to, from) {
      return Ok(vec![Statement::DuckConv {
        index: table,
        ptr: val,
        reg,
      }]);
    }
    return Err(BuildError::InvalidType(loc));
  }

  pub fn build_assign(&mut self, lhs: &AST, rhs: &AST) -> Result<Program, BuildError> {
    if lhs.is_pattern() {
      let mut program = vec![];
      let (mut prog, ty) = self.build_value_to_ret(rhs)?;
      program.append(&mut prog);
      program.push(Statement::Push { reg: REG_RETURN });
      let val = self.inc_stack_size();
      let skip = self.create_label();
      let pass = self.create_label();
      let mut pattern_program = self.build_param_pattern(lhs, val, ty, skip)?;
      program.append(&mut pattern_program);
      program.push(Statement::Jmp { label: pass });
      program.push(Statement::Label { name: skip });
      // TODO: Stack return empty
      program.push(Statement::Ret {});
      program.push(Statement::Label { name: pass });
      return Ok(program);
    }
    let (mut program, ty) = self.build_value_to_reg(rhs, REG_RETURN)?;
    if let AST::Variable(name, _) = lhs {
      if let Some(full) = self.type_by_name(name) {
        if !ty.satisfies(&full) {
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

  pub fn resolve_parameter_type(&mut self, lhs: &AST, rhs: &AST) -> Result<DataType, BuildError> {
    if lhs.is_pattern() {
      Ok(DataType::Unresolved)
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
    self.add_stack();
    if !generic.is_empty() {
      self.add_generic_scope(generic); // add generic resolved scope
    }
    for (lhs, rhs) in args {
      parsed_args.push(self.resolve_parameter_type(&lhs, &rhs)?);
    }

    let ret = match ret {
      AST::None(..) => DataType::None,
      ret => self.resolve_typedef(ret)?,
    };

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
    self.pop_stack();
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
     type b = bool
     type f = float
     type s = string
     fn e(a: i, {a: 1}): string{}
     fn e(a: i, b: map:[s, i]): string{}
    */
    let ast = AST::Block(vec![
      AST::Initialize(AssignType::Type, Box::new(v("i")), Box::new(v("int")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("b")), Box::new(v("bool")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("f")), Box::new(v("float")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("s")), Box::new(v("string")), l()),
      AST::Function("e".to_string(), vec![], vec![(v("a"), v("int")), (
                AST::MapLiteral(vec![(v("a"), AST::IntLiteral(1, l()))], l()), AST::None(l()))],
                Box::new(v("string")), Box::new(AST::Block(vec![], l())), l()),
                
      AST::Function("e".to_string(), vec![], vec![(v("a"), v("int")), (v("b"),
        AST::GenericIdentifier(Box::new(v("map")), vec![v("s"), v("i")], l()))],
                Box::new(v("string")), Box::new(AST::Block(vec![], l())), l()),
    ], l());

    let mut interpreter = Interpreter::new(ast);
    interpreter.build().unwrap();
    assert_eq!(interpreter.typedef_by_name(&"i".to_string()), Some(DataType::Int));
    assert_eq!(interpreter.typedef_by_name(&"b".to_string()), Some(DataType::Bool));
    assert_eq!(interpreter.typedef_by_name(&"f".to_string()), Some(DataType::Float));
    assert_eq!(interpreter.typedef_by_name(&"s".to_string()), Some(DataType::String));
    assert_eq!(interpreter.type_by_name(&"e".to_string()), Some(DataType::Function(Box::new(FunctionDataType {
      name: "e".to_string(),
      args: vec![DataType::Int, Interpreter::build_map_type(&DataType::String, &DataType::Int)],
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
    interpreter.build().unwrap();
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
    interpreter.build().unwrap();
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
              Box::new(
                AST::BinOp(Box::new(v("a")),
                BinOpType::Add,
                Box::new(v("b")),l())
              )
            ), l()
          )
        ],
      l())), l())
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

  #[test]
  fn test_duck_typing() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
      type A = struct {
        x: int,
        y: int,
      }
      type B = struct {
        y: int
      }

      fn c(b: B) {

      }

      fn e() {
        let a = A { x: 1, y: 2 }
        c(a)
      }
    */
    let ast = AST::Block(
      vec![
        AST::Initialize(
          AssignType::Type,
          Box::new(v("A")),
          Box::new(AST::StructDefinition(
            vec![],
            vec![("x".into(), v("int")), ("y".into(), v("int"))],
            l(),
          )),
          l(),
        ),
        AST::Initialize(
          AssignType::Type,
          Box::new(v("B")),
          Box::new(AST::StructDefinition(
            vec![],
            vec![("y".into(), v("int"))],
            l(),
          )),
          l(),
        ),
        AST::Function(
          "c".to_string(),
          vec![],
          vec![(v("b"), v("B"))],
          Box::new(AST::None(l())),
          Box::new(AST::Block(vec![], l())),
          l(),
        ),
        AST::Function(
          "e".to_string(),
          vec![],
          vec![],
          Box::new(AST::None(l())),
          Box::new(AST::Block(
            vec![
              AST::Initialize(
                AssignType::Let,
                Box::new(v("a")),
                Box::new(AST::StructLiteral(
                  Box::new(v("A")),
                  vec![
                    ("x".into(), AST::IntLiteral(1, l())),
                    ("y".into(), AST::IntLiteral(2, l())),
                  ],
                  l(),
                )),
                l(),
              ),
              AST::Call(Box::new(v("c")), vec![v("a")], l()),
            ],
            l(),
          )),
          l(),
        ),
      ],
      l(),
    );

    let mut interpreter = Interpreter::new(ast);
    interpreter.build().unwrap();
    assert_eq!(interpreter.duck_tables[0], vec![1]);
    println!("{:?}", interpreter.build().unwrap()); // Visual check iguess (^_^)
  }

  #[test]
  fn test_build_pattern() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
     fn e({a: [1, a]}) {
     }
     fn e(a: map:[string, array:[int]]) {
     }
    */
    let ast = AST::Block(
      vec![
        AST::Function(
          "e".to_string(),
          vec![],
          vec![(
            AST::MapLiteral(
              vec![(
                v("a"),
                AST::ArrayLiteral(vec![AST::IntLiteral(1, l()), v("a")], l()),
              )],
              l(),
            ),
            AST::None(l()),
          )],
          Box::new(AST::None(l())),
          Box::new(AST::Block(vec![], l())),
          l(),
        ),
        AST::Function(
          "e".to_string(),
          vec![],
          vec![(
            v("a"),
            AST::GenericIdentifier(
              Box::new(v("map")),
              vec![
                v("string"),
                AST::GenericIdentifier(Box::new(v("array")), vec![v("int")], l()),
              ],
              l(),
            ),
          )],
          Box::new(AST::None(l())),
          Box::new(AST::Block(vec![], l())),
          l(),
        ),
      ],
      l(),
    );

    let mut interpreter = Interpreter::new(ast);
    println!("{:?}", interpreter.build().unwrap()); // Visual check iguess (^_^)
  }
}
