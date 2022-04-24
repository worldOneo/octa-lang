use octa_lex::lexer::{self, Keyword, OpType};

#[derive(Debug, Clone, PartialEq)]
pub enum AssignType {
  Let,
  Const,
  Type,
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

impl BinOpType {
  pub fn is_comparison(&self) -> bool {
    match self {
      BinOpType::Eq
      | BinOpType::Ne
      | BinOpType::Lt
      | BinOpType::Gt
      | BinOpType::Le
      | BinOpType::Ge => true,
      _ => false,
    }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOpType {
  Neg,
  Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
  None(lexer::CodeLocation),
  Block(Vec<AST>, lexer::CodeLocation),
  Initialize(AssignType, Box<AST>, Box<AST>, lexer::CodeLocation),
  Assign(Box<AST>, Box<AST>, lexer::CodeLocation),
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
  StructDefinition(String, Vec<String>, Vec<(String, AST)>, lexer::CodeLocation),
  FunctionDefinition(
    String,
    Vec<String>,
    Vec<(AST, AST)>,
    Box<AST>,
    Box<AST>,
    lexer::CodeLocation,
  ),
  MemberAccess(Box<AST>, String, lexer::CodeLocation),
  IndexAccess(Box<AST>, Box<AST>, lexer::CodeLocation),
  If(Box<AST>, Box<AST>, Option<Box<AST>>, lexer::CodeLocation),
  Return(Option<Box<AST>>, lexer::CodeLocation),
  GenericIdentifier(String, Vec<AST>, lexer::CodeLocation), // Func[asd,asd,asd]
}

impl AST {
  pub fn location(&self) -> lexer::CodeLocation {
    match self {
      AST::None(loc) => loc,
      AST::Block(.., loc) => loc,
      AST::Initialize(.., loc) => loc,
      AST::Assign(.., loc) => loc,
      AST::BinOp(.., loc) => loc,
      AST::UnOp(.., loc) => loc,
      AST::Call(.., loc) => loc,
      AST::Variable(.., loc) => loc,
      AST::IntLiteral(.., loc) => loc,
      AST::FloatLiteral(.., loc) => loc,
      AST::StringLiteral(.., loc) => loc,
      AST::BoolLiteral(.., loc) => loc,
      AST::ArrayLiteral(.., loc) => loc,
      AST::MapLiteral(.., loc) => loc,
      AST::StructLiteral(.., loc) => loc,
      AST::StructDefinition(.., loc) => loc,
      AST::FunctionDefinition(.., loc) => loc,
      AST::MemberAccess(.., loc) => loc,
      AST::IndexAccess(.., loc) => loc,
      AST::If(.., loc) => loc,
      AST::Return(.., loc) => loc,
      AST::GenericIdentifier(.., loc) => loc,
    }
    .clone()
  }

  pub fn subtrees(&self) -> Vec<&AST> {
    match self {
      AST::None(_) => vec![],
      AST::Block(subtrees, _) => subtrees.iter().collect(),
      AST::Initialize(_, lhs, rhs, _) => vec![lhs, rhs],
      AST::Assign(lhs, rhs, _) => vec![lhs, rhs],
      AST::BinOp(lhs, _, rhs, _) => vec![lhs, rhs],
      AST::UnOp(_, operand, _) => vec![operand],
      AST::Call(callee, args, _) => vec![callee.as_ref()]
        .into_iter()
        .chain(args.iter())
        .collect(),
      AST::Variable(_, _) => vec![],
      AST::IntLiteral(_, _) => vec![],
      AST::FloatLiteral(_, _) => vec![],
      AST::StringLiteral(_, _) => vec![],
      AST::BoolLiteral(_, _) => vec![],
      AST::ArrayLiteral(elements, _) => elements.iter().collect::<Vec<_>>(),
      AST::MapLiteral(pairs, _) => pairs
        .iter()
        .map(|(a, _)| a)
        .chain(pairs.iter().map(|(_, b)| b))
        .collect::<Vec<_>>(),
      AST::StructLiteral(_, elements, _) => elements.iter().collect::<Vec<_>>(),
      AST::StructDefinition(_, _, elements, _) => {
        elements.iter().map(|(_, b)| b).collect::<Vec<_>>()
      }
      AST::FunctionDefinition(_, _, _, _, body, _) => vec![body],
      AST::MemberAccess(obj, _, _) => vec![obj],
      AST::IndexAccess(obj, index, _) => vec![obj, index],
      AST::If(cond, then, els, _) => {
        if let Some(els) = els {
          vec![cond, then, els]
        } else {
          vec![cond, then]
        }
      }
      AST::Return(Some(expr), _) => vec![expr],
      _ => vec![],
    }
  }

  pub fn is_pattern(&self) -> bool {
    match self {
      AST::IntLiteral(..)
      | AST::FloatLiteral(..)
      | AST::BoolLiteral(..)
      | AST::MapLiteral(..)
      | AST::ArrayLiteral(..)
      | AST::StringLiteral(..) => true,
      _ => false,
    }
  }
}

pub struct Parser {
  tokens: Vec<(lexer::Token, lexer::CodeLocation)>,
  pos: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum AstError {
  UnexpectedToken(lexer::Token, lexer::CodeLocation),
  TokenExpected(lexer::Token, Option<lexer::Token>, lexer::CodeLocation),
  IdentifierExpected(lexer::CodeLocation),
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
      lexer::Token::Operator(OpType::LBrace) => {
        let args = self.parse_map()?;
        self.parse_extension(AST::MapLiteral(args, loc))
      }
      _ => Err(AstError::UnexpectedToken(token, loc)),
    }
  }

  pub fn parse_call_parameters(&mut self) -> Result<Vec<AST>, AstError> {
    self.parse_list(lexer::Token::Operator(OpType::RParen))
  }

  pub fn possbile_operator(&mut self) -> Result<Option<BinOpType>, AstError> {
    let (op_tok, loc) = self.peek()?;
    let op = match op_tok {
      lexer::Token::Operator(op_tok) => op_tok,
      _ => return Err(AstError::UnexpectedToken(op_tok, loc)),
    };
    let next = self.peek_offset(1);
    let first_op = as_bin_op(&op);
    type Res = fn(BinOpType) -> (Result<Option<BinOpType>, AstError>, i32);
    let some: Res = |op: BinOpType| (Ok(Some(op)), 2);
    let other = || (Ok(first_op), 1);

    if next.is_err() {
      self.dequeue()?;
      return Ok(first_op);
    } else {
      let (token, _) = next.unwrap();
      if let lexer::Token::Operator(op2) = token {
        let (res, deq) = match op {
          OpType::Gt => match op2 {
            OpType::Eq => some(BinOpType::Ge),
            OpType::Gt => some(BinOpType::Shr),
            _ => other(),
          },
          OpType::Lt => match op2 {
            OpType::Eq => some(BinOpType::Le),
            OpType::Lt => some(BinOpType::Shl),
            _ => other(),
          },
          OpType::Or => match op2 {
            OpType::Or => some(BinOpType::Lor),
            _ => other(),
          },
          OpType::And => match op2 {
            OpType::And => some(BinOpType::Land),
            _ => other(),
          },
          OpType::Eq => match op2 {
            OpType::Eq => some(BinOpType::Eq),
            _ => other(),
          },
          OpType::Not => match op2 {
            OpType::Eq => some(BinOpType::Ne),
            _ => other(),
          },
          _ => other(),
        };
        if let Ok(Some(_)) = res {
          for _ in 0..deq {
            self.dequeue()?;
          }
        }
        return res;
      }
      if first_op.is_some() {
        self.dequeue()?;
      }
      Ok(first_op)
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
          let (token, rloc) = self.dequeue()?;
          if let lexer::Token::Operator(OpType::RBraket) = token {
            let ast = AST::IndexAccess(Box::new(ast), Box::new(arg), loc);
            self.parse_extension(ast)
          } else {
            Err(AstError::TokenExpected(
              lexer::Token::Operator(OpType::RBraket),
              Some(token),
              rloc,
            ))
          }
        }
        op_type if is_partial_bin_op(&op_type) => {
          if let Ok(Some(op)) = self.possbile_operator() {
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
            if op_type == OpType::Eq {
              let rhs = self.parse_value()?;
              return Ok(AST::Assign(Box::new(ast), Box::new(rhs), loc));
            }
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
      Ok(AST::Initialize(
        match keyword {
          lexer::Keyword::Let => AssignType::Let,
          lexer::Keyword::Const => AssignType::Const,
          lexer::Keyword::Type => AssignType::Type,
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

  pub fn parse_map(&mut self) -> Result<Vec<(AST, AST)>, AstError> {
    let mut map = Vec::new();
    loop {
      let key = self.parse_value()?;
      let (col, loc) = self.dequeue()?;
      if let lexer::Token::Operator(lexer::OpType::Colon) = col {
        let value = self.parse_value()?;
        map.push((key, value));
        let (comma, loc) = self.dequeue()?;
        if let lexer::Token::Operator(lexer::OpType::Comma) = comma {
        } else if let lexer::Token::Operator(lexer::OpType::RBrace) = comma {
          break;
        } else {
          return Err(AstError::TokenExpected(
            lexer::Token::Operator(lexer::OpType::Comma),
            Some(comma),
            loc,
          ));
        }
      } else {
        return Err(AstError::TokenExpected(
          lexer::Token::Operator(lexer::OpType::Colon),
          Some(col),
          loc,
        ));
      }
    }
    Ok(map)
  }

  pub fn parse_return(&mut self) -> Result<AST, AstError> {
    let (token, loc) = self.peek()?;
    if let lexer::Token::Operator(OpType::Semicolon) = token {
      self.dequeue()?;
      Ok(AST::Return(None, loc))
    } else {
      let ast = self.parse_value()?;
      Ok(AST::Return(Some(Box::new(ast)), loc))
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
        Keyword::Return => self.parse_return(),
        _ => match self.parse_value() {
          Ok(ast) => self.parse_extension(ast),
          Err(e) => Err(e),
        },
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

  fn parse_type(&mut self) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    match &token {
      lexer::Token::Identifier(identifier) => {
        let identifier = identifier.clone();
        let ident = match identifier.clone().as_str() {
          "int" | "bool" | "string" | "float" => AST::Variable(identifier.clone(), loc.clone()),
          "none" => AST::None(loc.clone()),
          _ => return Err(AstError::UnexpectedToken(token, loc)),
        };
        let (token, loc) = self.peek()?;
        if let lexer::Token::Operator(OpType::LBraket) = token {
          let types = self.parse_type_list()?;
          return Ok(AST::GenericIdentifier(identifier, types, loc));
        }
        return Ok(ident);
      }
      _ => Err(AstError::IdentifierExpected(loc.clone())),
    }
  }

  fn parse_type_list(&mut self) -> Result<Vec<AST>, AstError> {
    let mut types = Vec::new();
    loop {
      let (token, loc) = self.dequeue()?;
      if let lexer::Token::Operator(OpType::RBraket) = token {
        break;
      }
      types.push(self.parse_type()?);
      let (token, loc) = self.dequeue()?;
      if let lexer::Token::Operator(OpType::Comma) = token {
      } else if let lexer::Token::Operator(OpType::RBraket) = token {
        break;
      } else {
        return Err(AstError::TokenExpected(
          lexer::Token::Operator(OpType::Comma),
          Some(token),
          loc,
        ));
      }
    }
    Ok(types)
  }

  // Parses a function in the style of
  // fn foo {}
  //
  // fn foo(a: int, b: int) {}
  //
  // fn foo(a: int, b: int): int {}
  //
  // fn foo[T,V](a: T): V {}
  //
  // fn Type foo[T, V](a: T): V {}
  pub fn parse_function(&mut self, fn_loc: lexer::CodeLocation) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    if let lexer::Token::Identifier(name) = token {
      let (token, loc) = self.dequeue()?;
      match token {
        lexer::Token::Operator(OpType::LBrace) => {
          let body = self.parse_block(loc.clone())?;
          return Ok(AST::FunctionDefinition(
            name,
            vec![],
            vec![],
            Box::new(AST::None(fn_loc.clone())),
            Box::new(body),
            fn_loc,
          ));
        }
        lexer::Token::Operator(OpType::LBraket) => {
          let generics = self.parse_identifier_list(lexer::Token::Operator(OpType::RBraket))?;
          let (token, loc) = self.dequeue()?;
          if let lexer::Token::Operator(OpType::LParen) = token {
            let params = self.parse_function_params()?;
            let (token, loc) = self.dequeue()?;
            if let lexer::Token::Operator(OpType::Colon) = token {
              let return_type = self.parse_type()?;
              return Ok(AST::FunctionDefinition(
                name,
                generics,
                params,
                Box::new(return_type),
                Box::new(self.parse_full_body(loc.clone())?),
                loc,
              ));
            } else {
              return Ok(AST::FunctionDefinition(
                name,
                generics,
                params,
                Box::new(AST::None(fn_loc.clone())),
                Box::new(self.parse_full_body(loc.clone())?),
                loc,
              ));
            }
          } else {
            return Err(AstError::TokenExpected(
              lexer::Token::Operator(OpType::LParen),
              Some(token),
              loc,
            ));
          }
        }
        lexer::Token::Operator(OpType::LParen) => {
          let params = self.parse_function_params()?;
          let (token, loc) = self.dequeue()?;
          if let lexer::Token::Operator(OpType::Colon) = token {
            let return_type = self.parse()?;
            return Ok(AST::FunctionDefinition(
              name,
              vec![],
              params,
              Box::new(return_type),
              Box::new(self.parse_full_body(loc.clone())?),
              loc,
            ));
          } else {
            return Ok(AST::FunctionDefinition(
              name,
              vec![],
              params,
              Box::new(AST::None(fn_loc.clone())),
              Box::new(self.parse_full_body(loc.clone())?),
              loc,
            ));
          }
        }
        _ => {
          return Err(AstError::TokenExpected(
            lexer::Token::Operator(OpType::LBrace),
            Some(token),
            loc,
          ))
        }
      }
    } else {
      return Err(AstError::IdentifierExpected(loc));
    }
  }

  pub fn parse_function_params(&mut self) -> Result<Vec<(AST, AST)>, AstError> {
    let mut params = Vec::new();
    loop {
      let (token, _) = self.dequeue()?;
      if let lexer::Token::Operator(OpType::RParen) = token {
        break;
      }
      let name = self.parse()?;
      let (mut token, mut loc) = self.dequeue()?;
      if let lexer::Token::Operator(OpType::Colon) = token {
        (token, loc) = self.dequeue()?;
        let ty = self.parse_type()?;
        params.push((name, ty));
      } else {
        params.push((name, AST::None(loc.clone())));
      }
      if let lexer::Token::Operator(OpType::Comma) = token {
      } else if let lexer::Token::Operator(OpType::RParen) = token {
        break;
      } else {
        return Err(AstError::TokenExpected(
          lexer::Token::Operator(OpType::Comma),
          Some(token),
          loc,
        ));
      }
    }
    Ok(params)
  }

  pub fn parse_identifier_list(&mut self, end: lexer::Token) -> Result<Vec<String>, AstError> {
    let mut res = vec![];
    loop {
      let (token, loc) = self.dequeue()?;
      if token == end {
        break;
      }
      if let lexer::Token::Identifier(name) = token {
        res.push(name);
        let (token, loc) = self.dequeue()?;
        if let lexer::Token::Operator(OpType::Comma) = token {
        } else if token == end {
          break;
        } else {
          return Err(AstError::TokenExpected(
            lexer::Token::Operator(OpType::Comma),
            Some(token),
            loc,
          ));
        }
      } else {
        return Err(AstError::TokenExpected(
          lexer::Token::Identifier(String::new()),
          Some(token),
          loc,
        ));
      }
    }
    Ok(res)
  }

  fn parse_full_body(&mut self, loc: lexer::CodeLocation) -> Result<AST, AstError> {
    let (token, loc) = self.dequeue()?;
    if let lexer::Token::Operator(OpType::LBrace) = token {
      let body = self.parse_block(loc.clone())?;
      return Ok(body);
    } else {
      return Err(AstError::TokenExpected(
        lexer::Token::Operator(OpType::LBrace),
        Some(token),
        loc,
      ));
    }
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
      Ok(AST::Initialize(
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
  fn test_parse_member_access() {
    let l = || CodeLocation::new("base".to_string(), 0);
    let tokens = vec![
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Dot), l()),
      (Token::Identifier("y".to_string()), l()),
      (Token::Operator(OpType::Dot), l()),
      (Token::Identifier("z".to_string()), l()),
    ];
    let ast = Parser::new(tokens).parse();
    assert_eq!(
      ast,
      Ok(AST::Initialize(
        AssignType::Let,
        Box::new(AST::Variable("x".to_string(), l())),
        Box::new(AST::MemberAccess(
          Box::new(AST::MemberAccess(
            Box::new(AST::Variable("x".to_string(), l())),
            "y".to_string(),
            l(),
          )),
          "z".to_string(),
          l()
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
          vec![AST::Initialize(
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
            vec![AST::Initialize(
              AssignType::Let,
              Box::new(AST::Variable("x".to_string(), l())),
              Box::new(AST::FloatLiteral(3.0, l())),
              l(),
            ),],
            l(),
          )),
          Some(Box::new(AST::Block(
            vec![AST::Initialize(
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
          vec![AST::Initialize(
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

  #[test]
  fn test_parse_map() {
    let l = || CodeLocation::new("base".to_string(), 0);
    let tokens = vec![
      (Token::Keyword(Keyword::Let), l()),
      (Token::Identifier("x".to_string()), l()),
      (Token::Operator(OpType::Eq), l()),
      (Token::Operator(OpType::LBrace), l()),
      (Token::String("a".to_string()), l()),
      (Token::Operator(OpType::Colon), l()),
      (Token::Float(1.0), l()),
      (Token::Operator(OpType::Comma), l()),
      (Token::String("b".to_string()), l()),
      (Token::Operator(OpType::Colon), l()),
      (Token::Float(2.0), l()),
      (Token::Operator(OpType::RBrace), l()),
    ];
    let ast = Parser::new(tokens).parse();
    assert_eq!(
      ast,
      Ok(AST::Initialize(
        AssignType::Let,
        Box::new(AST::Variable("x".to_string(), l())),
        Box::new(AST::MapLiteral(
          vec![
            (
              AST::StringLiteral("a".to_string(), l()),
              AST::FloatLiteral(1.0, l()),
            ),
            (
              AST::StringLiteral("b".to_string(), l()),
              AST::FloatLiteral(2.0, l()),
            ),
          ],
          l(),
        )),
        l(),
      ))
    );
  }
}
