use std::iter::Map;

use octa_lex::lexer::{self, Keyword, OpType};

#[derive(Debug, Clone, PartialEq)]
pub enum AssignType {
  Let,
  Const,
}

pub struct StructField {
  pub name: String,
  pub ty: DataType,
}

pub struct StructType {
  pub name: String,
  pub fields: Map<String, StructField>,
}

pub struct FunctionDataType {
  pub name: String,
  pub args: Vec<DataType>,
  pub ret: DataType,
}

pub enum DataType {
  Int,
  Float,
  Bool,
  String,
  Array(Box<DataType>),
  Map(Box<DataType>, Box<DataType>),
  Struct(Box<StructType>),
  Function(Box<FunctionDataType>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOpType {
  Lor,
  Land,
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Eq,
  Ne,
  Lt,
  Gt,
  Le,
  Ge,
  And,
  Or,
  Xor,
  Shl,
  Shr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOpType {
  Neg,
  Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
  Block(Vec<AST>, lexer::CodeLocation),
  Assign(AssignType, Box<AST>, Box<AST>, lexer::CodeLocation),
  BinOp(Box<AST>, BinOpType, Box<AST>, lexer::CodeLocation),
  UnOp(UnOpType, Box<AST>, lexer::CodeLocation),
  Call(Box<AST>, Vec<AST>, lexer::CodeLocation),
  Variable(String, lexer::CodeLocation),
  IntLiteral(i64, lexer::CodeLocation),
  FloatLiteral(f64, lexer::CodeLocation),
  StringLiteral(String, lexer::CodeLocation),
  BoolLiteral(bool, lexer::CodeLocation),
  ArrayLiteral(Vec<AST>, lexer::CodeLocation),
  MapLiteral(Vec<(AST, AST)>, lexer::CodeLocation),
  StructLiteral(String, Vec<AST>, lexer::CodeLocation),
  StructDefinition(String, Vec<(String, String)>, lexer::CodeLocation),
  FunctionDefinition(
    String,
    Vec<(String, String)>,
    String,
    Box<AST>,
    lexer::CodeLocation,
  ),
  MemberAccess(Box<AST>, String, lexer::CodeLocation),
  IndexAccess(Box<AST>, Box<AST>, lexer::CodeLocation),
  If(Box<AST>, Box<AST>, Option<Box<AST>>, lexer::CodeLocation),
}

pub struct Parser {
  tokens: Vec<(lexer::Token, lexer::CodeLocation)>,
  pos: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstError {
  UnexpectedToken(lexer::Token, lexer::CodeLocation),
  TokenExpected(lexer::Token, Option<lexer::Token>, lexer::CodeLocation),
  AstEnd,
}

fn is_partial_bin_op(op: &OpType) -> bool {
  match op {
    OpType::Add => true,
    OpType::Sub => true,
    OpType::Mul => true,
    OpType::Div => true,
    OpType::Mod => true,
    OpType::Lt => true,
    OpType::Gt => true,
    OpType::And => true,
    OpType::Or => true,
    OpType::Xor => true,
    OpType::Eq => true,
    _ => false,
  }
}

fn as_bin_op(op: &OpType) -> Option<BinOpType> {
  match op {
    OpType::Add => Some(BinOpType::Add),
    OpType::Sub => Some(BinOpType::Sub),
    OpType::Mul => Some(BinOpType::Mul),
    OpType::Div => Some(BinOpType::Div),
    OpType::Mod => Some(BinOpType::Mod),
    OpType::Lt => Some(BinOpType::Lt),
    OpType::Gt => Some(BinOpType::Gt),
    OpType::And => Some(BinOpType::And),
    OpType::Or => Some(BinOpType::Or),
    OpType::Xor => Some(BinOpType::Xor),
    _ => None,
  }
}

impl Parser {
  pub fn new(tokens: Vec<(lexer::Token, lexer::CodeLocation)>) -> Parser {
    Parser { tokens, pos: 0 }
  }

  pub fn dequeue(&mut self) -> Result<(lexer::Token, lexer::CodeLocation), AstError> {
    self.pos += 1;
    if self.pos - 1 >= self.tokens.len() {
      Err(AstError::AstEnd)
    } else {
      Ok(self.tokens[self.pos - 1].clone())
    }
  }

  pub fn peek(&self) -> Result<(lexer::Token, lexer::CodeLocation), AstError> {
    self.peek_offset(0)
  }

  pub fn peek_offset(&self, i: usize) -> Result<(lexer::Token, lexer::CodeLocation), AstError> {
    if self.pos + i >= self.tokens.len() {
      Err(AstError::AstEnd)
    } else {
      Ok(self.tokens[self.pos + i].clone())
    }
  }

  pub fn parse_list(&mut self, end: lexer::Token) -> Result<Vec<AST>, AstError> {
    let mut args = Vec::new();
    loop {
      let val = self.dequeue()?;
      match val {
        (lexer::Token::Operator(OpType::Comma), _) => {
          args.push(self.parse_value()?);
        }
        (token, _) if token == end => {
          return Ok(args);
        }
        (token, pos) => {
          return Err(AstError::TokenExpected(
            lexer::Token::Operator(OpType::RParen),
            Some(token),
            pos,
          ));
        }
      }
    }
  }

  pub fn parse_value(&mut self) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    match token {
      lexer::Token::Integer(i) => self.parse_extension(AST::IntLiteral(i, loc)),
      lexer::Token::Float(f) => self.parse_extension(AST::FloatLiteral(f, loc)),
      lexer::Token::String(s) => self.parse_extension(AST::StringLiteral(s, loc)),
      lexer::Token::Bool(b) => self.parse_extension(AST::BoolLiteral(b, loc)),
      lexer::Token::Identifier(s) => self.parse_extension(AST::Variable(s, loc)),
      lexer::Token::Operator(OpType::LParen) => {
        let args = self.parse_list(lexer::Token::Operator(OpType::RParen))?;
        self.parse_extension(AST::ArrayLiteral(args, loc))
      }
      _ => Err(AstError::UnexpectedToken(token, loc)),
    }
  }

  pub fn parse_call_parameters(&mut self) -> Result<Vec<AST>, AstError> {
    self.parse_list(lexer::Token::Operator(OpType::RParen))
  }

  pub fn extend_operator(&self, op: OpType, peek_offset: usize) -> Option<BinOpType> {
    let next = self.peek_offset(peek_offset);
    let other = as_bin_op(&op);
    if next.is_err() {
      return other;
    } else {
      let (token, _) = next.unwrap();
      if let lexer::Token::Operator(op2) = token {
        return match op {
          OpType::Gt => match op2 {
            OpType::Eq => Some(BinOpType::Ge),
            OpType::Gt => Some(BinOpType::Shr),
            _ => other,
          },
          OpType::Lt => match op2 {
            OpType::Eq => Some(BinOpType::Le),
            OpType::Lt => Some(BinOpType::Shl),
            _ => other,
          },
          OpType::Or => match op2 {
            OpType::Or => Some(BinOpType::Lor),
            _ => other,
          },
          OpType::And => match op2 {
            OpType::And => Some(BinOpType::Land),
            _ => other,
          },
          OpType::Eq => match op2 {
            OpType::Eq => Some(BinOpType::Eq),
            _ => other,
          },
          OpType::Not => match op2 {
            OpType::Eq => Some(BinOpType::Ne),
            _ => other,
          },
          _ => other,
        };
      }
      other
    }
  }

  pub fn parse_extension(&mut self, ast: AST) -> Result<AST, AstError> {
    let token = self.peek();
    match token {
      Err(AstError::AstEnd) => return Ok(ast),
      Err(e) => return Err(e),
      Ok(_) => (),
    };
    let (token, loc) = token.unwrap();
    match token.clone() {
      lexer::Token::Operator(op_type) => match op_type {
        OpType::Dot => {
          self.dequeue()?;
          let (token, loc) = self.dequeue()?;
          match token {
            lexer::Token::Identifier(name) => {
              let ast = AST::MemberAccess(Box::new(ast), name, loc);
              self.parse_extension(ast)
            }
            token => Err(AstError::TokenExpected(
              lexer::Token::Identifier("<any>".to_string()),
              Some(token),
              loc,
            )),
          }
        }
        OpType::LParen => {
          self.dequeue()?;
          let args = self.parse_call_parameters()?;
          let ast = AST::Call(Box::new(ast), args, loc);
          self.parse_extension(ast)
        }
        OpType::LBraket => {
          self.dequeue()?;
          let arg = self.parse_value()?;
          if let lexer::Token::Operator(OpType::RBraket) = self.dequeue()?.0 {
            let ast = AST::IndexAccess(Box::new(ast), Box::new(arg), loc);
            self.parse_extension(ast)
          } else {
            Err(AstError::TokenExpected(
              lexer::Token::Operator(OpType::RBraket),
              Some(token),
              loc,
            ))
          }
        }
        op_type if is_partial_bin_op(&op_type) => {
          if let Some(op) = self.extend_operator(op_type, 1) {
            self.dequeue()?;
            self.dequeue()?;
            let rhs = self.parse_value()?;
            if let AST::BinOp(left, right_op, right, _) = rhs.clone() {
              if (op as isize) > (right_op as isize) {
                let left = AST::BinOp(Box::new(ast), op, left, loc.clone());
                let ast = AST::BinOp(Box::new(left), right_op, right, loc.clone());
                return self.parse_extension(ast);
              }
            }
            let ast = AST::BinOp(Box::new(ast), op, Box::new(rhs), loc);
            self.parse_extension(ast)
          } else {
            Ok(ast)
          }
        }
        _ => Ok(ast),
      },
      _ => Ok(ast),
    }
  }

  pub fn parse_initialization(&mut self, keyword: lexer::Keyword) -> Result<AST, AstError> {
    let left_hand = self.parse_value()?;
    let eq = self.dequeue()?;
    if let lexer::Token::Operator(lexer::OpType::Eq) = eq.0 {
      let right_hand = self.parse_value()?;
      Ok(AST::Assign(
        match keyword {
          lexer::Keyword::Let => AssignType::Let,
          lexer::Keyword::Const => AssignType::Const,
          _ => return Err(AstError::UnexpectedToken(eq.0, eq.1)),
        },
        Box::new(left_hand),
        Box::new(right_hand),
        eq.1,
      ))
    } else {
      Err(AstError::TokenExpected(
        lexer::Token::Operator(lexer::OpType::Eq),
        Some(eq.0),
        eq.1,
      ))
    }
  }

  pub fn parse_else(&mut self) -> Result<Option<AST>, AstError> {
    let else_ = self.peek();
    if let Err(AstError::AstEnd) = else_ {
      return Ok(None);
    }
    let (token, _) = else_.unwrap();
    match token {
      lexer::Token::Keyword(lexer::Keyword::Else) => {
        self.dequeue()?;
        let ast = self.parse()?;
        Ok(Some(ast))
      }
      _ => Ok(None),
    }
  }

  pub fn parse_if(&mut self) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    if let lexer::Token::Operator(lexer::OpType::LParen) = token {
      let cond = self.parse_value()?;
      let (token, loc) = self.dequeue()?;
      if let lexer::Token::Operator(lexer::OpType::RParen) = token {
        let then = self.parse();
        let then = then.unwrap();
        let else_ = self.parse_else()?;
        Ok(AST::If(
          Box::new(cond),
          Box::new(then),
          if let Some(else_) = else_ {
            Some(Box::new(else_))
          } else {
            None
          },
          loc,
        ))
      } else {
        Err(AstError::TokenExpected(
          lexer::Token::Operator(lexer::OpType::RParen),
          Some(token),
          loc,
        ))
      }
    } else {
      Err(AstError::TokenExpected(
        lexer::Token::Operator(lexer::OpType::LParen),
        Some(token),
        loc,
      ))
    }
  }

  pub fn parse(&mut self) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    if let lexer::Token::Operator(OpType::LBrace) = token {
      return self.parse_block(loc);
    }
    match token.clone() {
      lexer::Token::Keyword(keyword) => match keyword {
        Keyword::Const | Keyword::Let => self.parse_initialization(keyword.clone()),
        Keyword::If => self.parse_if(),
        _ => Err(AstError::UnexpectedToken(token, loc)),
      },
      _ => return Err(AstError::UnexpectedToken(token.clone(), loc.clone())),
    }
  }

  pub fn parse_block(&mut self, loc: lexer::CodeLocation) -> Result<AST, AstError> {
    let mut loc = loc;
    let mut ast = Vec::new();
    loop {
      let read = self.peek();
      match read {
        Err(AstError::AstEnd) => {
          return Err(AstError::TokenExpected(
            lexer::Token::Operator(OpType::RBrace),
            None,
            loc,
          ))
        }
        Err(e) => return Err(e),
        _ => (),
      }
      let (token, _loc) = read.unwrap();
      loc = _loc;
      if let lexer::Token::Operator(OpType::RBrace) = token {
        self.dequeue()?;
        break;
      }
      ast.push(self.parse()?);
    }
    Ok(AST::Block(ast, loc))
  }

  pub fn parse_all(&mut self) -> Result<AST, AstError> {
    let mut ast = Vec::new();
    loop {
      let read = self.peek();
      match read {
        Err(AstError::AstEnd) => break,
        Err(e) => return Err(e),
        _ => (),
      }
      ast.push(self.parse()?);
    }
    Ok(AST::Block(
      ast,
      lexer::CodeLocation::new("base".to_string(), 0),
    ))
  }
}

pub fn parse(tokens: Vec<(lexer::Token, lexer::CodeLocation)>) -> Result<AST, AstError> {
  Parser::new(tokens).parse()
}

#[cfg(test)]
mod tests {
  use super::*;
  use lexer::CodeLocation;
  use lexer::Keyword;
  use lexer::OpType;
  use lexer::Token;

  #[test]
  fn test_parse_value() {
    let l = || CodeLocation::new("base".to_string(), 0);
    let tokens = vec![
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(1.0), l()),
      (Token::Operator(OpType::Add), l()),
      (Token::Float(2.0), l()),
      (Token::Operator(OpType::Mul), l()),
      (Token::Float(3.0), l()),
    ];
    let ast = Parser::new(tokens).parse();
    assert_eq!(
      ast,
      Ok(AST::Assign(
        AssignType::Let,
        Box::new(AST::Variable("x".to_string(), l())),
        Box::new(AST::BinOp(
          Box::new(AST::FloatLiteral(1.0, l())),
          BinOpType::Add,
          Box::new(AST::BinOp(
            Box::new(AST::FloatLiteral(2.0, l())),
            BinOpType::Mul,
            Box::new(AST::FloatLiteral(3.0, l())),
            l(),
          )),
          l(),
        )),
        l(),
      ))
    );
  }

  #[test]
  fn test_parse_if_full() {
    let l = || CodeLocation::new("base".to_string(), 0);
    let tokens = vec![
      (Token::Keyword(Keyword::If), l()),
      (Token::Operator(OpType::LParen), l()),
      (Token::Float(1.0), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(2.0), l()),
      (Token::Operator(OpType::RParen), l()),
      (Token::Operator(OpType::LBrace), l()),
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(3.0), l()),
      (Token::Operator(OpType::RBrace), l()),
      (Token::Keyword(Keyword::Else), l()),
      (Token::Keyword(Keyword::If), l()),
      (Token::Operator(OpType::LParen), l()),
      (Token::Float(1.0), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(2.0), l()),
      (Token::Operator(OpType::RParen), l()),
      (Token::Operator(OpType::LBrace), l()),
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(3.0), l()),
      (Token::Operator(OpType::RBrace), l()),
      (Token::Keyword(Keyword::Else), l()),
      (Token::Operator(OpType::LBrace), l()),
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(3.0), l()),
      (Token::Operator(OpType::RBrace), l()),
    ];
    let ast = Parser::new(tokens).parse();
    assert_eq!(
      ast,
      Ok(AST::If(
        Box::new(AST::BinOp(
          Box::new(AST::FloatLiteral(1.0, l())),
          BinOpType::Eq,
          Box::new(AST::FloatLiteral(2.0, l())),
          l(),
        )),
        Box::new(AST::Block(
          vec![AST::Assign(
            AssignType::Let,
            Box::new(AST::Variable("x".to_string(), l())),
            Box::new(AST::FloatLiteral(3.0, l())),
            l(),
          ),],
          l(),
        )),
        Some(Box::new(AST::If(
          Box::new(AST::BinOp(
            Box::new(AST::FloatLiteral(1.0, l())),
            BinOpType::Eq,
            Box::new(AST::FloatLiteral(2.0, l())),
            l(),
          )),
          Box::new(AST::Block(
            vec![AST::Assign(
              AssignType::Let,
              Box::new(AST::Variable("x".to_string(), l())),
              Box::new(AST::FloatLiteral(3.0, l())),
              l(),
            ),],
            l(),
          )),
          Some(Box::new(AST::Block(
            vec![AST::Assign(
              AssignType::Let,
              Box::new(AST::Variable("x".to_string(), l())),
              Box::new(AST::FloatLiteral(3.0, l())),
              l(),
            ),],
            l(),
          ))),
          l(),
        ))),
        l(),
      ))
    );
  }

  #[test]
  fn parse_if_partial() {
    let l = || CodeLocation::new("base".to_string(), 0);
    let tokens = vec![
      (Token::Keyword(Keyword::If), l()),
      (Token::Operator(OpType::LParen), l()),
      (Token::Float(1.0), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(2.0), l()),
      (Token::Operator(OpType::RParen), l()),
      (Token::Operator(OpType::LBrace), l()),
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Float(3.0), l()),
      (Token::Operator(OpType::RBrace), l()),
    ];
    let ast = Parser::new(tokens).parse();
    assert_eq!(
      ast,
      Ok(AST::If(
        Box::new(AST::BinOp(
          Box::new(AST::FloatLiteral(1.0, l())),
          BinOpType::Eq,
          Box::new(AST::FloatLiteral(2.0, l())),
          l(),
        )),
        Box::new(AST::Block(
          vec![AST::Assign(
            AssignType::Let,
            Box::new(AST::Variable("x".to_string(), l())),
            Box::new(AST::FloatLiteral(3.0, l())),
            l(),
          ),],
          l(),
        )),
        None,
        l(),
      ))
    );
  }
}
