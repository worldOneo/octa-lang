use std::num::ParseFloatError;

pub struct Lexer {
  code: Vec<char>,
  file: String,
  line: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpType {
  LBraket,
  RBraket,
  LCBraket,
  RCBraket,
  LSBraket,
  RSBraket,
  Question,
  Comma,
  Not,
  Dot,
  Add,
  Sub,
  Div,
  Mul,
  Mod,
  And,
  Xor,
  Eq,
  Gt,
  Lt,
  Or,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
  Fn,
  If,
  Else,
  For,
  Struct,
  Let,
  Const,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeLocation {
  file: String,
  line: usize,
}

impl CodeLocation {
  pub fn new(file: String, line: usize) -> CodeLocation {
    CodeLocation { file, line }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
  Operator(OpType),
  Identifier(String),
  Integer(i64),
  Float(f64),
  String(String),
  Keyword(Keyword),
}

fn as_keyword(id: String) -> Option<Keyword> {
  match id.as_str() {
    "fn" => Some(Keyword::Fn),
    "if" => Some(Keyword::If),
    "else" => Some(Keyword::Else),
    "for" => Some(Keyword::For),
    "struct" => Some(Keyword::Struct),
    "let" => Some(Keyword::Let),
    "const" => Some(Keyword::Const),
    _ => None,
  }
}

fn as_operator(c: char) -> Option<OpType> {
  match c {
    '(' => Some(OpType::LBraket),
    ')' => Some(OpType::RBraket),
    '{' => Some(OpType::LCBraket),
    '}' => Some(OpType::RCBraket),
    '[' => Some(OpType::LSBraket),
    ']' => Some(OpType::RSBraket),
    '+' => Some(OpType::Add),
    '-' => Some(OpType::Sub),
    '/' => Some(OpType::Div),
    '*' => Some(OpType::Mul),
    '%' => Some(OpType::Mod),
    '&' => Some(OpType::And),
    '^' => Some(OpType::Xor),
    '=' => Some(OpType::Eq),
    '>' => Some(OpType::Gt),
    '<' => Some(OpType::Lt),
    '|' => Some(OpType::Or),
    '?' => Some(OpType::Question),
    '!' => Some(OpType::Not),
    _ => None,
  }
}

fn is_whitespace(c: char) -> bool {
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

fn is_line_terminator(c: char) -> bool {
  c == '\n'
}

fn is_numeric(c: char) -> bool {
  c >= '0' && c <= '9'
}

fn parse_int(s: &str) -> Result<i64, String> {
  let mut i = 0;
  for c in s.chars() {
    if !is_numeric(c) {
      if c == '_' {
        continue;
      }
      return Err("Invalid integer".to_string());
    }
    i *= 10;
    i += (c as i64) - ('0' as i64);
  }
  Ok(i)
}

#[derive(Debug, Clone, PartialEq)]
pub enum LexerError {
  InvalidCharacter(CodeLocation),
  InvalidInteger(CodeLocation),
  InvalidFloat(CodeLocation),
  InvalidKeyword(CodeLocation),
  InvalidOperator(CodeLocation),
  InvalidString(CodeLocation),
  EndOfCode,
}

fn parse_float(s: &str) -> Result<f64, String> {
  let mut i = 0.0;
  let mut fraction = false;
  let mut partial = 1.0;
  for c in s.chars() {
    if !is_numeric(c) {
      if c == '_' {
        continue;
      }
      if c == '.' {
        if fraction {
          return Err("Invalid float".to_string());
        }
        fraction = true;
        continue;
      }
      return Err("Invalid float".to_string());
    }
    if fraction {
      partial /= 10.0;
      i += ((c as i64 as f64) - ('0' as i64 as f64)) * partial;
    } else {
      i *= 10.0;
      i += (c as i64 as f64) - ('0' as i64 as f64);
    }
  }
  Ok(i)
}

impl Lexer {
  pub fn new(code: &str, file: &str) -> Lexer {
    Lexer {
      code: code.chars().collect::<Vec<char>>(),
      file: file.to_string(),
      line: 1,
    }
  }

  fn pull_identifier(&mut self, c: char) -> String {
    let mut identifier = String::new();
    identifier.push(c);
    while let Some(c) = self.code.get(0) {
      if !c.is_alphanumeric() {
        break;
      }
      identifier.push(*c);
      self.code.remove(0);
    }
    identifier
  }

  fn pull_integer(&mut self, c: char) -> Result<i64, String> {
    let mut integer = String::new();
    integer.push(c);
    while let Some(c) = self.code.get(0) {
      if !(is_numeric(*c) || *c == '_') {
        break;
      }
      integer.push(*c);
      self.code.remove(0);
    }
    parse_int(integer.as_str())
  }

  fn pull_float(&mut self, c: char) -> Result<f64, String> {
    let mut float = String::new();
    float.push(c);
    while let Some(c) = self.code.get(0) {
      if !(is_numeric(*c) || *c == '_' || *c == '.') {
        break;
      }
      float.push(*c);
      self.code.remove(0);
    }
    parse_float(float.as_str())
  }

  fn pull_string(&mut self) -> Result<String, String> {
    let mut string = String::new();
    while let Some(c) = self.code.get(0) {
      if *c == '"' {
        break;
      }
      string.push(*c);
      self.code.remove(0);
    }
    if let Some(c) = self.code.get(0) {
      if *c == '"' {
        self.code.remove(0);
        return Ok(string);
      }
    }
    Err("Unterminated string".to_string())
  }

  pub fn location(&self) -> CodeLocation {
    CodeLocation {
      file: self.file.clone(),
      line: self.line,
    }
  }

  pub fn pull_token(&mut self) -> Result<Token, LexerError> {
    let first = self.code.get(0);
    if first.is_none() {
      return Err(LexerError::EndOfCode);
    }
    let first = *first.unwrap();
    self.code.remove(0);
    if is_whitespace(first) {
      if is_line_terminator(first) {
        self.line += 1;
      }
      return self.pull_token();
    }
    if let Some(op) = as_operator(first) {
      return Ok(Token::Operator(op));
    }

    if is_numeric(first) {
      if let Ok(integer) = self.pull_integer(first) {
        return Ok(Token::Integer(integer));
      }
      if let Ok(float) = self.pull_float(first) {
        return Ok(Token::Float(float));
      }
      return Err(LexerError::InvalidInteger(self.location()));
    }

    if first == '"' {
      if let Ok(string) = self.pull_string() {
        return Ok(Token::String(string));
      }
      return Err(LexerError::InvalidString(self.location()));
    }
    let identifier = self.pull_identifier(first);
    if let Some(keyword) = as_keyword(identifier.clone()) {
      return Ok(Token::Keyword(keyword));
    }

    return Ok(Token::Identifier(identifier));
  }

  pub fn all_tokens(&mut self) -> Vec<(Token, CodeLocation)> {
    let mut tokens = vec![];
    while let Ok(token) = self.pull_token() {
      tokens.push((token, self.location()));
    }
    tokens
  }
}

#[cfg(test)]
mod tests {
  use crate::lexer::LexerError;

  #[test]
  fn test_parse_int() {
    assert_eq!(super::parse_int("123"), Ok(123));
    assert_eq!(super::parse_int("123_456"), Ok(123456));
    assert_eq!(super::parse_int("_123"), Ok(123));
    assert_eq!(super::parse_int("_123_456"), Ok(123456));
    assert_eq!(super::parse_int("_123_456_"), Ok(123456));
    assert_eq!(super::parse_int("_123_456_789"), Ok(123456789));
    assert_eq!(super::parse_int("_123_456_789_"), Ok(123456789));
    assert_eq!(super::parse_int("_123_456_789_0"), Ok(1234567890));
  }

  #[test]
  fn test_parse_float() {
    assert_eq!(super::parse_float("123.456"), Ok(123.456));
    assert_eq!(super::parse_float("123.456_789"), Ok(123.456_789));
    assert_eq!(super::parse_float("_123.456"), Ok(123.456));
    assert_eq!(super::parse_float("_123.456_789"), Ok(123.456_789));
    assert_eq!(super::parse_float("_123.456_789_"), Ok(123.456_789));
    assert_eq!(super::parse_float("_123.456_789_0"), Ok(123.456_789));
  }

  #[test]
  // uses lexer to parse operators
  fn test_parse_operators() {
    let mut lexer = super::Lexer::new("+-*/%", "test");
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Add))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Sub))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Mul))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Div))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Mod))
    );
    assert_eq!(lexer.pull_token(), Err(LexerError::EndOfCode));
  }

  #[test]
  fn test_lexer_parse_calculation() {
    let mut lexer = super::Lexer::new("1 + 2 * 3", "test");
    assert_eq!(lexer.pull_token(), Ok(super::Token::Integer(1)));
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Add))
    );
    assert_eq!(lexer.pull_token(), Ok(super::Token::Integer(2)));
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Mul))
    );
    assert_eq!(lexer.pull_token(), Ok(super::Token::Integer(3)));
    assert_eq!(lexer.pull_token(), Err(LexerError::EndOfCode));
  }

  #[test]
  fn test_lexer_parse_keywords() {
    let mut lexer = super::Lexer::new("if else", "test");
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Keyword(super::Keyword::If))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Keyword(super::Keyword::Else))
    );
    assert_eq!(lexer.pull_token(), Err(LexerError::EndOfCode));
  }

  #[test]
  fn test_lexer_parse_assignment() {
    let mut lexer = super::Lexer::new("let a = 1", "test");
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Keyword(super::Keyword::Let))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Identifier("a".to_string()))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Eq))
    );
    assert_eq!(lexer.pull_token(), Ok(super::Token::Integer(1)));
    assert_eq!(lexer.pull_token(), Err(LexerError::EndOfCode));
  }

  #[test]
  fn test_lexex_parse_string() {
    let mut lexer = super::Lexer::new("let a = \"hello world\"", "test");
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Keyword(super::Keyword::Let))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Identifier("a".to_string()))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::Operator(super::OpType::Eq))
    );
    assert_eq!(
      lexer.pull_token(),
      Ok(super::Token::String("hello world".to_string()))
    );
    assert_eq!(lexer.pull_token(), Err(LexerError::EndOfCode));
  }

  #[test]
  #[rustfmt::skip]
  fn test_lexer_all_tokens() {
    let file = || "test".to_string();
    let mut lexer = super::Lexer::new(
      "let first = 1 + 2 * 3\r\nlet second = first\nlet third = 3",
      "test"
    );
    assert_eq!(
      lexer.all_tokens(),
      vec![
        (super::Token::Keyword(super::Keyword::Let), super::CodeLocation::new(file(), 1)),
        (super::Token::Identifier("first".to_string()), super::CodeLocation::new(file(), 1)),
        (super::Token::Operator(super::OpType::Eq), super::CodeLocation::new(file(), 1)),
        (super::Token::Integer(1), super::CodeLocation::new(file(), 1)),
        (super::Token::Operator(super::OpType::Add), super::CodeLocation::new(file(), 1)),
        (super::Token::Integer(2), super::CodeLocation::new(file(), 1)),
        (super::Token::Operator(super::OpType::Mul), super::CodeLocation::new(file(), 1)),
        (super::Token::Integer(3), super::CodeLocation::new(file(), 1)),
        (super::Token::Keyword(super::Keyword::Let), super::CodeLocation::new(file(), 2)),
        (super::Token::Identifier("second".to_string()), super::CodeLocation::new(file(), 2)),
        (super::Token::Operator(super::OpType::Eq), super::CodeLocation::new(file(), 2)),
        (super::Token::Identifier("first".to_string()), super::CodeLocation::new(file(), 2)),
        (super::Token::Keyword(super::Keyword::Let), super::CodeLocation::new(file(), 3)),
        (super::Token::Identifier("third".to_string()), super::CodeLocation::new(file(), 3)),
        (super::Token::Operator(super::OpType::Eq), super::CodeLocation::new(file(), 3)),
        (super::Token::Integer(3), super::CodeLocation::new(file(), 3)),
      ]
    )
  }
}
