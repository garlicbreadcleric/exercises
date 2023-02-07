// https://www.codewars.com/kata/55f89832ac9a66518f000118

use std::collections::HashMap;

pub fn simplify(input: &str) -> String {
  let mut simplifier = Simplifier::new(input);
  simplifier.parse();
  simplifier.show()
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Sign {
  Plus,
  Minus,
}

impl Sign {
  pub fn from_char(c: char) -> Option<Sign> {
    match c {
      '+' => Some(Sign::Plus),
      '-' => Some(Sign::Minus),
      _ => None,
    }
  }

  pub fn to_i32(&self) -> i32 {
    match self {
      Sign::Plus => 1,
      Sign::Minus => -1,
    }
  }
}

pub struct Simplifier<'a> {
  input: &'a str,
  size: usize,
  offset: usize,
  values: HashMap<String, i32>,
}

impl<'a> Simplifier<'a> {
  pub fn new(input: &str) -> Simplifier {
    Simplifier {
      input,
      size: input.chars().count(),
      offset: 0,
      values: HashMap::new(),
    }
  }

  fn parse(&mut self) {
    while self.offset < self.size {
      self.parse_term();
    }
  }

  fn parse_term(&mut self) {
    let sign = match self.peek() {
      None => {
        return;
      }
      Some(c) => {
        if let Some(s) = Sign::from_char(c) {
          self.offset += 1;
          s
        } else if self.offset == 0 {
          Sign::Plus
        } else {
          panic!("Invalid input!");
        }
      }
    };

    let mut digits = String::new();
    while let Some(c) = self.peek() {
      if c.is_ascii_digit() {
        digits.push(c);
        self.offset += 1;
      } else {
        break;
      }
    }
    let multiplier: i32 = digits.parse().unwrap_or(1);

    let mut letters = String::new();
    while let Some(c) = self.peek() {
      if c.is_ascii_alphabetic() {
        letters.push(c);
        self.offset += 1;
      } else {
        break;
      }
    }
    let mut letters: Vec<_> = letters.chars().collect();
    letters.sort();
    let letters: String = letters.iter().collect();

    let old_multiplier = *self.values.get(&letters).unwrap_or(&0);
    self
      .values
      .insert(letters, old_multiplier + multiplier * sign.to_i32());
  }

  fn peek(&self) -> Option<char> {
    self.input.chars().nth(self.offset)
  }

  fn show(&self) -> String {
    let mut keys: Vec<_> = self.values.keys().collect();
    keys.sort_by(|a, b| {
      if a.len() != b.len() {
        usize::cmp(&a.len(), &b.len())
      } else {
        String::cmp(a, b)
      }
    });

    let mut result = String::new();
    for key in keys {
      let multiplier = *self.values.get(key).unwrap_or(&0);
      if multiplier == 0 {
        continue;
      }

      if multiplier < 0 {
        result.push('-');
      } else if multiplier > 0 && !result.is_empty() {
        result.push('+');
      }

      if multiplier.abs() != 1 {
        result += &multiplier.abs().to_string();
      }
      result += key;
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn sample_tests() {
    assert_eq!(simplify("dc+dcba"), "cd+abcd");
    assert_eq!(simplify("2xy-yx"), "xy");
    assert_eq!(simplify("-a+5ab+3a-c-2a"), "-c+5ab");
    assert_eq!(simplify("-abc+3a+2ac"), "3a+2ac-abc");
    assert_eq!(simplify("xyz-xz"), "-xz+xyz");
    assert_eq!(simplify("a+ca-ab"), "a-ab+ac");
    assert_eq!(simplify("xzy+zby"), "byz+xyz");
    assert_eq!(simplify("-y+x"), "x-y");
    assert_eq!(simplify("y-x"), "-x+y");
    assert_eq!(simplify("3a+b+4ac+bc-ab+3a-cb-a-a"), "4a+b-ab+4ac");
    assert_eq!(
      simplify("+n-5hn+7tjhn-4nh-3n-6hnjt+2jhn+9hn"),
      "-2n+2hjn+hjnt"
    );
    assert_eq!(
      simplify("-8fk+5kv-4yk+7kf-qk+yqv-3vqy+4ky+4kf+yvqkf"),
      "3fk-kq+5kv-2qvy+fkqvy"
    );
    assert_eq!(simplify("-15cb-12cb-0c+7cb"), "-20bc");
    assert_eq!(
      simplify("-12dy+9yzd-9dyz-13y+8y-10yzd-11yd+15yd+9y"),
      "4y-8dy-10dyz"
    );
    assert_eq!(simplify("+11x+11x+0xd-12x+5adx+4xd"), "10x+4dx+5adx");
    assert_eq!(simplify("-0axz-0xz+0axz+0x+4xaz+14x+14zax"), "14x+18axz");
  }
}
