use std::{
  collections::{HashMap, HashSet},
  vec,
};

use octa_lex::lexer;
use octa_parse::parser::{self, AssignType, BinOpType, UnOpType, AST};

use crate::{optimizer::Optimizer, stack::Stack};

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
  Struct {
    id: usize,
    ty: StructType,
  },
  Function {
    ty: Box<FunctionDataType>,
  },
  Generic {
    name: String,
  },
  GenericResolved {
    solve: HashMap<String, DataType>,
    ty: Box<DataType>,
  },
  Const(Box<DataType>),
}

impl DataType {
  pub fn prog_eq(&self, other: &DataType) -> bool {
    match (self, other) {
      (DataType::Unresolved, DataType::Unresolved) => true,
      (DataType::None, DataType::None) => true,
      (DataType::Int, DataType::Int) => true,
      (DataType::Float, DataType::Float) => true,
      (DataType::Bool, DataType::Bool) => true,
      (DataType::String, DataType::String) => true,
      (DataType::Struct { ty: ty1, .. }, DataType::Struct { ty: ty2, .. }) => {
        if ty1.special != ty2.special {
          return false;
        }
        if ty1.fields.len() != ty2.fields.len() {
          return false;
        }
        for (name, (_, field1)) in &ty1.fields {
          if let Some((_, field2)) = &ty2.fields.get(name) {
            if !field1.ty.prog_eq(&field2.ty) {
              return false;
            }
          } else {
            return false;
          }
        }
        true
      }
      (DataType::Function { ty: ty1 }, DataType::Function { ty: ty2 }) => {
        if ty1.name != ty2.name {
          return false;
        }
        if ty1.args.len() != ty2.args.len() {
          return false;
        }
        for (arg1, arg2) in ty1.args.iter().zip(ty2.args.iter()) {
          if !arg1.prog_eq(arg2) {
            return false;
          }
        }
        return ty1.ret.prog_eq(&ty2.ret);
      }
      (DataType::Generic { name: name1 }, DataType::Generic { name: name2 }) => name1 == name2,
      (
        DataType::GenericResolved {
          solve: solve1,
          ty: ty1,
        },
        DataType::GenericResolved {
          solve: solve2,
          ty: ty2,
        },
      ) => {
        if solve1.len() != solve2.len() {
          return false;
        }
        for (name, ty) in solve1.iter() {
          if let Some(ty2) = solve2.get(name) {
            if !ty.prog_eq(ty2) {
              return false;
            }
          } else {
            return false;
          }
        }
        ty1.prog_eq(ty2)
      }
      (DataType::Const(ty1), DataType::Const(ty2)) => ty1.prog_eq(ty2),
      _ => false,
    }
  }

  pub fn satisfies(&self, other: &DataType) -> bool {
    if self == other {
      return true;
    }
    if let DataType::Const(inner) = self {
      return inner.satisfies(other);
    } else if let DataType::Const(other) = other {
      return self.satisfies(other);
    }
    if let DataType::Struct { ty: inner, .. } = self {
      if let DataType::Struct {
        ty: other_inner, ..
      } = other
      {
        return other_inner
          .fields
          .iter()
          .all(|(name, ty)| inner.fields.contains_key(name) && inner.fields[name].1.ty == ty.1.ty);
      }
    }
    if let DataType::Function { ty: inner } = self {
      if let DataType::Function { ty: other_inner } = other {
        return inner.args.len() == other_inner.args.len()
          && inner
            .args
            .iter()
            .zip(other_inner.args.iter())
            .all(|(a, b)| a.prog_eq(&b))
          && inner.ret.prog_eq(&other_inner.ret);
      }
    }

    return false;
  }

  pub fn get_generic_names(&self) -> Vec<String> {
    match self {
      DataType::GenericResolved { solve, .. } => solve.keys().into_iter().cloned().collect(),
      DataType::Struct { ty, .. } => ty.generic_types.clone(),
      DataType::Function { ty, .. } => ty.as_ref().generic_types.clone(),
      _ => vec![],
    }
  }

  pub fn is_const(&self) -> bool {
    match self {
      DataType::Const(_) => true,
      _ => false,
    }
  }

  pub fn is_primitive(&self) -> bool {
    match self {
      DataType::Int
      | DataType::Float
      | DataType::Bool
      | DataType::String
      | DataType::Function { .. } => true,
      _ => false,
    }
  }

  pub fn type_id(&self) -> usize {
    match self {
      DataType::Unresolved => 0,
      DataType::None => 1,
      DataType::Int => 2,
      DataType::Float => 3,
      DataType::Bool => 4,
      DataType::String => 5,
      DataType::Struct { id, .. } => *id,
      _ => todo!(),
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
    reg: Register,
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
  CaptureClosure {
    elements: usize,
    label: usize,
    reg: Register,
  },
  NilReg(Register),
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
    size: usize,
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
    from: usize,
    to: usize,
  },
  CallNative {
    name: String,
  },
  StackDec {
    size: usize,
  },
}
const REG_A: Register = 0;
const REG_B: Register = 1;
const REG_C: Register = 2;
const REG_RETURN: Register = 255;

pub type Program = Vec<Statement>;

pub struct Interpreter {
  pub syntax: AST,
  pub type_count: usize,
  pub duck_table_index: HashMap<Vec<usize>, usize>,
  pub duck_tables: Vec<Vec<usize>>,
  pub label_count: usize,
  pub stacks: Vec<Stack>,
  pub stack_functions: Vec<HashMap<String, (Function, usize)>>,
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
  OutOfScopeAccess(lexer::CodeLocation),
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
      type_count: 100,
      label_count: 0,
      syntax,
      duck_table_index: HashMap::new(),
      duck_tables: Vec::new(),
      stacks: vec![Stack::new()],
      stack_functions: vec![HashMap::new()],
    };
    let map = interpreter.build_map_type(
      &DataType::Generic { name: "K".into() },
      &DataType::Generic { name: "V".into() },
    );
    let array = interpreter.build_array_type(&DataType::Generic { name: "T".into() });
    let stack = &mut interpreter.stacks[0];
    stack.set_typedef("int".to_string(), DataType::Int);
    stack.set_typedef("float".into(), DataType::Float);
    stack.set_typedef("bool".into(), DataType::Bool);
    stack.set_typedef("string".into(), DataType::String);
    stack.set_typedef("map".into(), map);
    stack.set_typedef("array".into(), array);
    interpreter
  }

  fn next_type_id(&mut self) -> usize {
    self.type_count += 1;
    self.type_count
  }

  fn build_map_type(&mut self, k: &DataType, v: &DataType) -> DataType {
    DataType::Struct {
      id: self.next_type_id(),
      ty: StructType {
        special: STRUCT_SPECIAL_MAP,
        fields: vec![
          ("k".into(), (0, StructField { ty: k.clone() })),
          ("v".into(), (1, StructField { ty: v.clone() })),
        ]
        .into_iter()
        .collect(),
        sorted_fields: vec![
          ("k".into(), StructField { ty: k.clone() }),
          ("v".into(), StructField { ty: v.clone() }),
        ],
        generic_types: vec!["K".to_string(), "V".to_string()],
        is_generic: true,
      },
    }
  }

  fn as_map_key_val(map: &DataType) -> Option<(DataType, DataType)> {
    match map {
      DataType::Struct {
        ty: StructType {
          special,
          sorted_fields,
          ..
        },
        ..
      } => {
        if *special == STRUCT_SPECIAL_MAP {
          return Some((sorted_fields[0].1.ty.clone(), sorted_fields[1].1.ty.clone()));
        }
        None
      }
      _ => None,
    }
  }

  fn as_array_val(array: &DataType) -> Option<DataType> {
    match array {
      DataType::Struct {
        ty: StructType {
          special,
          sorted_fields: fields,
          ..
        },
        ..
      } => {
        if *special != STRUCT_SPECIAL_ARR {
          return None;
        }
        return Some(fields[0].1.ty.clone());
      }
      _ => None,
    }
  }

  fn build_array_type(&mut self, t: &DataType) -> DataType {
    DataType::Struct {
      id: self.next_type_id(),
      ty: StructType {
        special: STRUCT_SPECIAL_ARR,
        fields: vec![("t".into(), (0, StructField { ty: t.clone() }))]
          .into_iter()
          .collect(),
        sorted_fields: vec![("t".into(), StructField { ty: t.clone() })],
        generic_types: vec!["T".to_string()],
        is_generic: true,
      },
    }
  }

  pub fn get_visible_scopes(&self) -> &Vec<HashMap<String, DataType>> {
    &self.stacks.last().unwrap().data_types
  }

  pub fn get_current_scope(&self) -> &HashMap<String, DataType> {
    let scopes = self.get_visible_scopes();
    &scopes.last().unwrap()
  }

  pub fn get_visible_offsets(&self) -> &Vec<HashMap<String, usize>> {
    &self.stacks.last().unwrap().offsets
  }

  pub fn type_by_name(&self, name: &String) -> Option<DataType> {
    // TODO: Capture variable
    for stack in self.stacks.iter().rev() {
      if let Some(ty) = stack.type_by_name(name) {
        return Some(ty.clone());
      }
    }
    None
  }

  pub fn typedef_by_name(&self, name: &String) -> Option<DataType> {
    for stacks in self.stacks.iter().rev() {
      if let Some(ty) = stacks.typedef_by_name(name) {
        return Some(ty.clone());
      }
    }
    None
  }

  pub fn set_typedef(&mut self, name: &String, ty: DataType) {
    self
      .stacks
      .last_mut()
      .unwrap()
      .set_typedef(name.clone(), ty);
  }

  fn create_label(&mut self) -> usize {
    self.label_count += 1;
    self.label_count
  }

  pub fn offset_by_name(&mut self, name: &String) -> Option<usize> {
    for scope in self.get_visible_offsets().iter().rev() {
      if let Some(offset) = scope.get(name) {
        return Some(*offset);
      }
    }
    let stacks_max = self.stacks.len() - 1;
    for (dirty, scope) in self.stacks.iter().rev().skip(1).enumerate() {
      if let Some(offset) = scope.offset_by_name(name) {
        let mut to = self.stacks[stacks_max - dirty].increase_stack_size();
        let mut from = offset;
        for next_stack in (stacks_max - dirty)..self.stacks.len() {
          self.stacks[next_stack].mark_dirty(from, to);
          from = to;
          to = self.stacks[next_stack].increase_stack_size();
        }
        return Some(to);
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
      DataType::Struct {
        ty: struct_type, ..
      } => {
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
        .ok_or(BuildError::TokenDoesntExist(loc.clone())),
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

        Ok(DataType::Struct {
          id: self.next_type_id(),
          ty: StructType {
            special: 0,
            fields: type_fields,
            sorted_fields: sorted_fields,
            generic_types: generics.clone(),
            is_generic: !generics.is_empty(),
          },
        })
      }
      AST::GenericIdentifier(ty, resolved, loc) => {
        let ty = self.resolve_typedef(ty)?;
        let ty = self.resolve_generic_type(&ty, resolved, loc)?;
        Ok(self.destruct_generic_resolved(&ty))
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
      DataType::Function { ty: func } => {
        if func.generic_types.len() != args_ty.len() {
          return Err(BuildError::GenericTypeMismatch(loc.clone()));
        }
        for (i, ty) in func.generic_types.iter().enumerate() {
          resolved_ty.insert(ty.clone(), args_ty[i].clone());
        }
        return Ok(DataType::GenericResolved {
          solve: resolved_ty,
          ty: Box::new(ty.clone()),
        });
      }
      DataType::Struct { ty: stru, .. } => {
        if stru.generic_types.len() != args_ty.len() {
          return Err(BuildError::GenericTypeMismatch(loc.clone()));
        }
        for (i, ty) in stru.generic_types.iter().enumerate() {
          resolved_ty.insert(ty.clone(), args_ty[i].clone());
        }
        return Ok(DataType::GenericResolved {
          solve: resolved_ty,
          ty: Box::new(ty.clone()),
        });
      }
      _ => return Err(BuildError::GenericTypeMismatch(loc.clone())),
    }
  }

  pub fn destruct_generic_resolved(&mut self, ty: &DataType) -> DataType {
    match ty {
      DataType::GenericResolved { solve, ty, .. } => {
        self.add_scope();
        for (key, val) in solve.iter() {
          self.set_typedef(key, val.clone());
        }
        let ty = self.destruct_generic_resolved(ty);
        self.pop_scope();
        return ty;
      }
      DataType::Struct { id, ty, .. } => {
        let fields = ty.fields.clone();
        let sorted_fields = ty.sorted_fields.clone();
        let sorted_fields = sorted_fields
          .iter()
          .map(|(name, field)| {
            (
              name.clone(),
              StructField {
                ty: self.destruct_generic_resolved(&field.ty),
              },
            )
          })
          .collect();
        let fields = fields
          .into_iter()
          .map(|(name, field)| {
            (
              name.clone(),
              (
                field.0,
                StructField {
                  ty: self.destruct_generic_resolved(&field.1.ty),
                },
              ),
            )
          })
          .collect();

        return DataType::Struct {
          id: *id,
          ty: StructType {
            special: ty.special,
            fields,
            sorted_fields,
            generic_types: ty.generic_types.clone(),
            is_generic: ty.is_generic,
          },
        };
      }
      DataType::Function { ty: func, .. } => {
        let mut args = func.args.clone();
        for i in 0..args.len() {
          args[i] = self.destruct_generic_resolved(&args[i]);
        }
        return DataType::Function {
          ty: Box::new(FunctionDataType {
            name: func.name.clone(),
            args,
            ret: self.destruct_generic_resolved(&func.ret),
            generic_types: func.generic_types.clone(),
            is_generic: func.is_generic,
          }),
        };
      }
      DataType::Generic { name, .. } => {
        return self
          .typedef_by_name(name)
          .unwrap_or(DataType::Generic { name: name.clone() });
      }
      _ => ty.clone(),
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
        if let DataType::Function { ty: func } = callee_ty {
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
        let key_ty = self.create_lowest_applicable_type(&keys);
        let val_ty = self.create_lowest_applicable_type(&vals);
        if key_ty == DataType::None || val_ty == DataType::None {
          return Err(BuildError::TypeNotInferable(loc.clone()));
        }
        Ok(self.build_map_type(&key_ty, &val_ty))
      }
      AST::ArrayLiteral(vals, loc) => {
        let mut vals_ty = vec![];
        for val in vals {
          vals_ty.push(self.resolve_type(val)?);
        }
        let ty = self.create_lowest_applicable_type(&vals_ty);
        if ty == DataType::None {
          return Err(BuildError::TypeNotInferable(loc.clone()));
        }
        Ok(self.build_array_type(&ty))
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
        let ty = self.resolve_generic_type(&ty, resolved, loc)?;
        Ok(self.destruct_generic_resolved(&ty))
      }
      AST::IntLiteral(_, _) => Ok(DataType::Int),
      AST::FloatLiteral(_, _) => Ok(DataType::Float),
      AST::StringLiteral(_, _) => Ok(DataType::String),
      AST::BoolLiteral(_, _) => Ok(DataType::Bool),
      _ => Err(BuildError::InvalidType(ast.location())),
    }
  }

  pub fn create_type_intersection(&mut self, first: &DataType, second: &DataType) -> DataType {
    if first == second {
      return first.clone();
    }
    if first == &DataType::Unresolved {
      return second.clone();
    }
    if second == &DataType::Unresolved {
      return first.clone();
    }
    if let DataType::Struct { ty: first_ty, .. } = first {
      if let DataType::Struct { ty: second_ty, .. } = second {
        let mut fields = HashMap::new();
        let mut sorted_fields = vec![];
        for (field_name, _) in &first_ty.fields {
          if let Some(field_type) = second_ty.fields.get(field_name) {
            fields.insert(field_name.clone(), field_type.clone());
            sorted_fields.push((field_name.clone(), field_type.1.clone()));
          }
        }
        return DataType::Struct {
          id: self.next_type_id(),
          ty: StructType {
            special: 0,
            fields,
            sorted_fields,
            generic_types: vec![],
            is_generic: false,
          },
        };
      }
    }
    return DataType::None;
  }

  pub fn create_lowest_applicable_type(&mut self, types: &Vec<DataType>) -> DataType {
    types.iter().fold(types[0].clone(), |acc, ty| {
      self.create_type_intersection(&acc, ty)
    })
  }

  pub fn build(&mut self) -> Result<Program, BuildError> {
    let syntax = &self.syntax.clone();
    let program = self.build_statement(syntax)?;
    let optimizer = Optimizer::new(program);
    let program = optimizer.optimize();
    return Ok(program);
  }

  fn inc_stack_size(&mut self) -> usize {
    self.stacks.last_mut().unwrap().increase_stack_size()
  }

  fn pop_stack(&mut self, loc: lexer::CodeLocation) -> Result<(), BuildError> {
    let stack = self.stacks.pop().unwrap();
    if stack.dirty_access.len() != 0 {
      return Err(BuildError::OutOfScopeAccess(loc.clone()));
    }
    Ok(())
  }

  fn pop_dirty_stack(&mut self) -> HashSet<(usize, usize)> {
    let stack = self.stacks.pop().unwrap();
    stack.dirty_access
  }

  fn pop_scope(&mut self) {
    self.stacks.last_mut().unwrap().pop_scope();
  }

  fn add_stack(&mut self) {
    self.stacks.push(Stack::new());
  }

  fn add_scope(&mut self) {
    self.stacks.last_mut().unwrap().push_scope();
  }

  fn stack_add(&mut self, name: &str) -> usize {
    self.stacks.last_mut().unwrap().add_var(name.into())
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
    self
      .stacks
      .last_mut()
      .unwrap()
      .set_datatype(name.clone(), ty);
  }

  fn build_fn_to_reg(
    &mut self,
    callee: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    match callee {
      AST::Variable(name, _) => {
        for stack in self.stack_functions.iter().rev() {
          if let Some((func, label)) = stack.get(name) {
            return Ok((
              vec![Statement::CaptureClosure {
                elements: 0,
                label: *label,
                reg: reg,
              }],
              DataType::Function {
                ty: Box::new(func.data_type.clone()),
              },
            ));
          }
        }
        Err(BuildError::InvalidCall(callee.location()))
      }
      AST::GenericIdentifier(..) => {
        let (prog, ty) = self.build_generic_identifier_to_reg(callee, reg)?;
        if let DataType::Function { .. } = ty {
          return Ok((prog, ty));
        }
        Err(BuildError::InvalidCall(callee.location()))
      }
      _ => {
        if let AST::Closure(..) = callee {
          let (program, ty) = self.build_closure_to_reg(callee, reg)?;
          return Ok((program, ty));
        }
        Err(BuildError::InvalidCall(callee.location()))
      }
    }
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
            if let Some((f, usize)) = self.stack_function(name) {
              return Ok((
                vec![Statement::CaptureClosure {
                  elements: 0,
                  label: usize,
                  reg,
                }],
                DataType::Function {
                  ty: Box::new(f.data_type.clone()),
                },
              ));
            }
            return Err(BuildError::InvalidType(value.location()));
          }
        } else {
          Err(BuildError::InvalidType(value.location()))?
        }
      }
      AST::StructLiteral(..) => return self.build_struct_literal_to_reg(value, reg),
      AST::BinOp(lhs, op, rhs, _) => self.build_bin_op(lhs, op.clone(), rhs, reg),
      AST::GenericIdentifier(..) => self.build_generic_identifier_to_reg(value, reg),
      AST::MapLiteral(..) => self.build_map_literal_to_reg(value, reg),
      AST::Nil(..) => Ok((vec![Statement::NilReg(reg)], DataType::None)),
      AST::Closure(..) => self.build_closure_to_reg(value, reg),
      _ => todo!(),
    }
  }

  fn build_map_literal_to_reg(
    &mut self,
    map: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    match map {
      AST::MapLiteral(kv, ..) => {
        let mut prog = vec![];
        for (key, value) in kv {
          let (mut key_prog, ..) = self.build_value_to_reg(key, REG_A)?;
          prog.append(&mut key_prog);
          prog.push(Statement::Push { reg: REG_A });
          let (mut value_prog, ..) = self.build_value_to_reg(value, REG_A)?;
          prog.append(&mut value_prog);
          prog.push(Statement::Push { reg: REG_A });
        }
        prog.push(Statement::MapInit {
          size: kv.len(),
          reg,
        });
        prog.push(Statement::StackDec { size: kv.len() * 2 });
        Ok((prog, self.resolve_type(&map)?))
      }
      _ => panic!("build_map_literal_to_reg expected AST::MapLiteral"),
    }
  }

  fn build_generic_identifier_to_reg(
    &mut self,
    value: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    match value {
      AST::GenericIdentifier(ty, generic_args, _) => {
        self.add_scope();
        let generic_names = self.resolve_type(ty.as_ref())?;
        for (arg, name) in generic_args
          .iter()
          .zip(generic_names.get_generic_names().iter())
        {
          let def = self.resolve_typedef(arg)?;
          self.set_data_type(name, def);
        }
        let res = self.build_value_to_reg(ty, reg);
        self.pop_scope();
        res
      }
      _ => panic!("build_generic_identifier_to_reg expected AST::GenericIdentifier"),
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
    if let DataType::Function { ty: func } = self.resolve_type(callee)? {
      if args.len() != func.args.len() {
        return Err(BuildError::ParameterLenMismatch(
          args.len(),
          func.args.len(),
          callee.location(),
        ));
      }
      let mut program = vec![];
      self.build_fn_to_reg(&callee, REG_A)?;
      program.push(Statement::Pop { reg: REG_A });
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
      program.push(Statement::Pop { reg: REG_A });
      program.push(Statement::Call { reg: REG_A });
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
        if let Some((key_ty, val_ty)) = Self::as_map_key_val(&ty) {
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
          if let DataType::Function { .. } = &ty {
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
      program.append(&mut self.build_patterned_function(function, *label)?);
    }
    self.stack_functions.pop();
    return Ok(program);
  }

  fn add_generic_scope(&mut self, generic: &Vec<String>) {
    self.add_scope();
    for arg in generic.iter() {
      let arg_type = DataType::Generic { name: arg.clone() };
      self.set_typedef(arg, arg_type);
    }
  }

  fn build_patterned_function(
    &mut self,
    func: &Function,
    label: usize,
  ) -> Result<Program, BuildError> {
    if !func.is_complete() {
      return Err(BuildError::FunctionError(
        FunctionCompleteErr::NoCompleteFunction,
        func.ast.location(),
      ));
    }
    let fn_guard = self.create_label();
    let mut program = vec![
      Statement::Jmp { label: fn_guard },
      Statement::Label { name: label },
    ];
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
      self.pop_stack(func.ast.location())?;
    }
    program.push(Statement::NilReg(REG_RETURN));
    program.push(Statement::Ret);
    program.push(Statement::Label { name: fn_guard });
    Ok(program)
  }

  fn build_fn(&mut self, ast: &AST) -> Result<(String, Function), BuildError> {
    match ast {
      AST::Function(name, generics, args, ret, body, loc) => {
        let ty = self.build_fn_def(name.to_string(), &generics, &args, &ret, &body, loc.clone())?;
        if let DataType::Function { ty: fn_ty } = ty {
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
      _ => panic!("build_fn ast is not a function"),
    }
  }

  fn build_closure_to_reg(
    &mut self,
    ast: &AST,
    reg: Register,
  ) -> Result<(Program, DataType), BuildError> {
    match ast {
      AST::Closure(generic, args, ret, body, loc) => {
        let ty = self.build_fn_def("".into(), generic, args, ret, body, loc.clone())?;
        let mut program = vec![];
        let mut capture = vec![];
        let skip = self.create_label();
        let fn_label = self.create_label();
        program.push(Statement::Jmp { label: skip });
        program.push(Statement::Label { name: fn_label });
        self.add_stack();
        self.add_generic_scope(&generic);
        for (i, arg) in args.iter().enumerate() {
          if let AST::Variable(id, ..) = &arg.0 {
            let arg_ty = self.resolve_typedef(&arg.1)?;
            self.set_data_type(&id, arg_ty);
            self.stack_add(&id);
          } else {
            return Err(BuildError::ParameterTypeMismatch(
              DataType::None,
              DataType::Unresolved,
              arg.1.location(),
            ));
          }
          program.push(Statement::Push { reg: i as u8 });
        }
        let mut body = &mut self.build_statement(body)?;
        self.pop_scope();
        let dirty = self.pop_dirty_stack();
        let mut dirty = dirty.iter().collect::<Vec<_>>();
        dirty.sort_by(|a, b| a.1.cmp(&b.1));
        for (from, _) in &dirty {
          capture.push(Statement::CopyToReg {
            reg: REG_A,
            offset: *from,
          });
          capture.push(Statement::Push { reg: REG_A });
        }

        for (reg_off, (_, to)) in dirty.iter().enumerate() {
          program.push(Statement::CopyFromReg {
            reg: (args.len() + reg_off) as u8,
            offset: *to,
          })
        }
        program.append(&mut body);
        program.push(Statement::NilReg(REG_RETURN));
        program.push(Statement::Ret);
        program.push(Statement::Label { name: skip });
        program.append(&mut capture);
        program.push(Statement::CaptureClosure {
          elements: dirty.len(),
          label: fn_label,
          reg,
        });
        program.push(Statement::StackDec { size: dirty.len() });
        Ok((program, ty))
      }
      _ => panic!("build_closure_to_reg ast is not a closure"),
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
    if let DataType::Struct { ty: strct, .. } = reach {
      if let DataType::Struct { ty: duck_strct, .. } = duck {
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
        if let DataType::Struct { ty: struct_ty, .. } = &ty {
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
      DataType::Struct { ty: strct, .. } => {
        let mut program = vec![];
        if let Some(_) = Self::as_array_val(ty) {
          program.push(Statement::ArrayInit { reg, size: 0 });
          return Ok(program);
        } else if let Some((_, _)) = Self::as_map_key_val(ty) {
          program.push(Statement::MapInit { reg, size: 0 });
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

  fn is_runtime_primitive(&self, ty: &DataType) -> bool {
    if let Some(_) = Self::as_array_val(ty) {
      return true;
    }
    if let Some((_, _)) = Self::as_map_key_val(ty) {
      return true;
    }
    return ty.is_primitive();
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
    if self.is_runtime_primitive(from) || from == to {
      return Ok(vec![Statement::Dupe { val, reg }]);
    }
    if let Some(table) = self.build_satisfaction_table(to, from) {
      return Ok(vec![Statement::DuckConv {
        index: table,
        ptr: val,
        reg,
        from: from.type_id(),
        to: to.type_id(),
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

  pub fn stack_function(&self, name: &String) -> Option<(Function, usize)> {
    for func in self.stack_functions.iter().rev() {
      if func.contains_key(name) {
        return Some(func[name].clone());
      }
    }
    None
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

    let current = self.typedef_by_name(&name);

    if let Some(current) = current {
      if let DataType::Function { ty: current } = current {
        if current.args.len() != parsed_args.len() {
          return Err(BuildError::ParameterLenMismatch(
            current.args.len(),
            parsed_args.len(),
            loc.clone(),
          ));
        }
        for (i, arg) in current.args.iter().enumerate() {
          let ty = self.create_type_intersection(arg, &parsed_args[i]);
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
    self.pop_stack(loc)?;
    Ok(DataType::Function {
      ty: Box::new(FunctionDataType {
        name,
        args: parsed_args,
        ret,
        is_generic: !generic.is_empty(),
        generic_types: generic.clone(),
      }),
    })
  }
}

#[cfg(test)]
mod tests {
  use std::cell::RefCell;

  use octa_parse::parser;

  use super::*;
  macro_rules! assert_dt {
    ($a:expr, $b:expr) => {
      assert!($a.is_some());
      assert!($b.is_some());
      assert!($a.unwrap().prog_eq(&$b.unwrap()));
    };
  }

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
    assert_dt!(interpreter.typedef_by_name(&"i".into()), Some(DataType::Int));
    assert_dt!(interpreter.typedef_by_name(&"b".into()), Some(DataType::Bool));
    assert_dt!(interpreter.typedef_by_name(&"f".into()), Some(DataType::Float));
    assert_dt!(interpreter.typedef_by_name(&"s".into()), Some(DataType::String));
    assert_dt!(interpreter.type_by_name(&"e".into()), Some(DataType::Function { ty: Box::new(FunctionDataType {
      name: "e".to_string(),
      args: vec![DataType::Int, Interpreter::build_map_type(&mut interpreter, &DataType::String, &DataType::Int)],
      ret: DataType::String,
      is_generic: false,
      generic_types: vec![],
    })}));
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
      DataType::Function{ty: Box::new(FunctionDataType {
        name: "e".to_string(),
        args: vec![DataType::Generic { name: "T".into() }],
        ret: DataType::Generic { name: "T".into() },
        is_generic: true,
        generic_types: vec!["T".to_string()],
      }
    )}));
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
    assert_dt!(
      interpreter.typedef_by_name(&"i".to_string()),
      Some(DataType::Function {
        ty: Box::new(FunctionDataType {
          name: "".to_string(),
          args: vec![DataType::Int],
          ret: DataType::Int,
          generic_types: vec!["T".to_string()],
          is_generic: true
        })
      })
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
  fn test_generic_identifier() {
    let l = RefCell::new(0);
    let l = move || {
      l.replace_with(|l| *l + 1);
      lexer::CodeLocation::new("".to_string(), *l.borrow())
    };
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
      fn map_get:[K,V](map: map:[K,V], key: K): T {

      }

      fn test() {
        let map = { x: 1, y: 2 }
        let result = map_get:[string, int](map, "x")
      }
    */
    let ast = AST::Block(
      vec![
        AST::Function(
          "map_get".to_string(),
          vec!["K".into(), "V".into()],
          vec![
            (
              v("map"),
              AST::GenericIdentifier(Box::new(v("map")), vec![v("K"), v("V")], l()),
            ),
            (v("key"), v("K")),
          ],
          Box::new(v("V")),
          Box::new(AST::Block(vec![], l())),
          l(),
        ),
        AST::Function(
          "test".to_string(),
          vec![],
          vec![],
          Box::new(AST::None(l())),
          Box::new(AST::Block(
            vec![
              AST::Initialize(
                AssignType::Let,
                Box::new(v("map")),
                Box::new(AST::MapLiteral(
                  vec![
                    (AST::StringLiteral("x".into(), l()), AST::IntLiteral(1, l())),
                    (AST::StringLiteral("y".into(), l()), AST::IntLiteral(2, l())),
                  ],
                  l(),
                )),
                l(),
              ),
              AST::Call(
                Box::new(AST::GenericIdentifier(
                  Box::new(v("map_get")),
                  vec![v("string"), v("int")],
                  l(),
                )),
                vec![v("map"), AST::StringLiteral("x".into(), l())],
                l(),
              ),
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

  #[test]
  fn test_build_closure() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    /*
      fn test() {
        let a = 1;
        let b = fn (): int { return a; }
      }
    */
    let ast = AST::Block(
      vec![AST::Function(
        "test".to_string(),
        vec![],
        vec![],
        Box::new(AST::None(l())),
        Box::new(AST::Block(
          vec![
            AST::Initialize(
              AssignType::Let,
              Box::new(v("a")),
              Box::new(AST::IntLiteral(1, l())),
              l(),
            ),
            AST::Initialize(
              AssignType::Let,
              Box::new(v("b")),
              Box::new(AST::Closure(
                vec![],
                vec![],
                Box::new(v("int")),
                Box::new(AST::Return(Some(Box::new(v("a"))), l())),
                l(),
              )),
              l(),
            ),
          ],
          l(),
        )),
        l(),
      )],
      l(),
    );
    let mut interpreter = Interpreter::new(ast);
    println!("{:?}", interpreter.build().unwrap()); // Visual check iguess (^_^)
  }
}
