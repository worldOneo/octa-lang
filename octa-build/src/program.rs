use std::{collections::HashMap, num::IntErrorKind};

use octa_lex::lexer;
use octa_parse::parser::{AssignType, AstError, BinOpType, UnOpType, AST};

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

pub enum Statement {
  Jmp,
  JmpIf,
  Call,
  Ret,
  Push { reg: Register },
  Swap,
  Dupe,
  Pop { reg: Register },
  Copy,
  Op { op: BinOpType },
  UnOp { op: UnOpType },
  SetIntReg(i64, Register),
  SetFloatReg(f64, Register),
  SetBoolReg(bool, Register),
  SetStringReg(String, Register),
}

pub type Program = Vec<Statement>;

pub struct Interpreter {
  pub syntax: AST,
  pub scope: Vec<HashMap<String, DataType>>,
  pub stack_offsets: Vec<HashMap<String, usize>>,
  pub data_types: Vec<HashMap<String, DataType>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BuildError {
  LefthandSideExpectedName(lexer::CodeLocation),
  InvalidType(lexer::CodeLocation),
  PatternExpectd(lexer::CodeLocation),
  TypeHasNoMember(lexer::CodeLocation),
  ParameterTypeMismatch(DataType, DataType, lexer::CodeLocation),
  ReturnTypeMismatch(DataType, DataType, lexer::CodeLocation),
  ParameterZeroIntersection(usize, lexer::CodeLocation),
  ParameterLenMismatch(usize, usize, lexer::CodeLocation),
  InvalidCall(lexer::CodeLocation),
  TypeNotInferable(lexer::CodeLocation),
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
      scope: vec![HashMap::new()],
      stack_offsets: vec![HashMap::new()],
      data_types: vec![HashMap::new()],
    };
    interpreter.data_types[0].insert("int".to_string(), DataType::Int);
    interpreter.data_types[0].insert("float".to_string(), DataType::Float);
    interpreter.data_types[0].insert("bool".to_string(), DataType::Bool);
    interpreter.data_types[0].insert("string".to_string(), DataType::String);
    interpreter
  }

  pub fn type_by_name(&self, name: &String) -> Option<DataType> {
    for scope in self.data_types.iter().rev() {
      if let Some(ty) = scope.get(name) {
        return Some(ty.clone());
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
          return Ok(ty.ty.clone());
        }
        Err(BuildError::TypeHasNoMember(loc))
      }
      _ => Err(BuildError::TypeHasNoMember(loc)),
    }
  }

  pub fn resolve_type_def(&self, ast: &AST) -> Result<DataType, BuildError> {
    match ast {
      AST::Variable(name, loc) => self
        .type_by_name(name)
        .ok_or(BuildError::InvalidType(loc.clone())),
      AST::MemberAccess(first, second, loc) => {
        let first_ty = self.resolve_type_def(first)?;
        self.get_member_type(first_ty, second, loc.clone())
      }
      AST::StructDefinition(name, generics, fields, loc) => {
        let mut type_fields = HashMap::new();
        for field in fields {
          let field_name = field.0.clone();
          let field_type = self.resolve_type_def(&field.1)?;
          type_fields.insert(field_name, StructField { ty: field_type });
        }
        Ok(DataType::Struct(StructType {
          name: name.clone(),
          fields: type_fields,
          generic_types: generics.clone(),
          is_generic: !generics.is_empty(),
        }))
      }
      AST::Call(callee, args, loc) => {
        let callee_ty = self.resolve_type_def(callee)?;
        if let DataType::Function(func) = callee_ty {
          let mut args_ty = vec![];
          for arg in args {
            args_ty.push(self.resolve_type_def(arg)?);
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
            keys.push(self.resolve_type_def(&kv.0)?);
          }
          vals.push(self.resolve_type_def(&kv.1)?);
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
          vals_ty.push(self.resolve_type_def(val)?);
        }
        let ty = Interpreter::create_lowest_applicable_type(&vals_ty);
        if ty == DataType::None {
          return Err(BuildError::TypeNotInferable(loc.clone()));
        }
        Ok(DataType::Array(Box::new(ty)))
      }
      AST::BinOp(first, op, second, loc) => {
        let first = self.resolve_type_def(first)?;
        let second = self.resolve_type_def(second)?;
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

  pub fn build(&mut self) -> Result<Program, BuildError> {
    let location = self.syntax.location();
    match self.syntax.clone() {
      AST::Block(definitions, loc) => {
        for definition in definitions {
          match definition {
            AST::Initialize(AssignType::Type, name, ty, loc) => {
              let ty = self.resolve_type_def(&ty)?;
              self.data_types[0].insert(as_name(&name, loc.clone())?, ty);
            }
            AST::FunctionDefinition(name, generic, args, ret, body, loc) => {
              self.add_fn_def(name, &generic, &args, &ret, &body, loc.clone())?;
            }
            _ => Err(BuildError::TopLevelOperationExpected(definition.location()))?,
          }
        }
      }
      _ => Err(BuildError::TopLevelOperationExpected(location))?,
    };
    return Ok(vec![]);
  }

  pub fn resolve_pattern_type(&self, pattern: &AST) -> Result<DataType, BuildError> {
    if !pattern.is_pattern() {
      return Err(BuildError::PatternExpectd(pattern.location()));
    }
    match pattern {
      AST::IntLiteral(..) => Ok(DataType::Int),
      AST::FloatLiteral(..) => Ok(DataType::Float),
      AST::BoolLiteral(..) => Ok(DataType::Bool),
      AST::StringLiteral(..) => Ok(DataType::String),
      AST::MapLiteral(..) | AST::ArrayLiteral(..) | AST::StructLiteral(..) => {
        self.resolve_type_def(&pattern)
      }
      _ => Err(BuildError::PatternExpectd(pattern.location())),
    }
  }

  pub fn resolve_parameter_type(&self, lhs: &AST, rhs: &AST) -> Result<DataType, BuildError> {
    if lhs.is_pattern() {
      self.resolve_pattern_type(lhs)
    } else {
      self.resolve_type_def(rhs)
    }
  }

  pub fn add_fn_def(
    &mut self,
    name: String,
    generic: &Vec<String>,
    args: &Vec<(AST, AST)>,
    ret: &AST,
    body: &AST,
    loc: lexer::CodeLocation,
  ) -> Result<(), BuildError> {
    let mut parsed_args = Vec::new();
    for (lhs, rhs) in args {
      parsed_args.push(self.resolve_parameter_type(&lhs, &rhs)?);
    }
    let ret = self.resolve_type_def(ret)?;
    let current = self.data_types[0].get(&name);
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
    self.data_types[0].insert(
      name.clone(),
      DataType::Function(Box::new(FunctionDataType {
        name,
        args: parsed_args,
        ret,
        is_generic: !generic.is_empty(),
        generic_types: generic.clone(),
      })),
    );
    Ok(())
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  #[rustfmt::skip]
  fn test_types() {
    let l = || lexer::CodeLocation::new("".to_string(), 0);
    let v = |s: &str| AST::Variable(s.to_string(), l());
    let ast = AST::Block(vec![
      AST::Initialize(AssignType::Type,  Box::new(v("a")), Box::new(v("int")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("b")), Box::new(v("bool")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("c")), Box::new(v("float")), l()),
      AST::Initialize(AssignType::Type, Box::new(v("d")), Box::new(v("string")), l()),
      AST::FunctionDefinition("e".to_string(), vec![], vec![(v("a"), v("int")), (
                AST::MapLiteral(vec![(v("a"), AST::IntLiteral(1, l()))], l()), AST::None(l()))],
                Box::new(v("string")), Box::new(AST::Block(vec![], l())), l())
    ], l());

    let mut interpreter = Interpreter::new(ast);
    interpreter.build().unwrap();
    assert_eq!(interpreter.data_types[0].get("a"), Some(&DataType::Int));
    assert_eq!(interpreter.data_types[0].get("b"), Some(&DataType::Bool));
    assert_eq!(interpreter.data_types[0].get("c"), Some(&DataType::Float));
    assert_eq!(interpreter.data_types[0].get("d"), Some(&DataType::String));
    assert_eq!(interpreter.data_types[0].get("e"), Some(&DataType::Function(Box::new(FunctionDataType {
      name: "e".to_string(),
      args: vec![DataType::Int, DataType::Map(Box::new(DataType::String), Box::new(DataType::Int))],
      ret: DataType::String,
      is_generic: false,
      generic_types: vec![],
    }))));
  }
}
