// https://www.codewars.com/kata/52a78825cdfc2cfc87000005

use std::{
  cell::RefCell,
  ops::{Deref, DerefMut},
  rc::Rc,
};

use ExprItem::*;
use ExprOp::*;
use ParserExpectation::*;

#[derive(Clone, Debug)]
pub enum ExprItem {
  Literal(f64),
  Parens(ExprRef),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExprOp {
  Add,
  Sub,
  Mul,
  Div,
}

impl ExprOp {
  pub fn from_char(c: char) -> Option<ExprOp> {
    match c {
      '+' => Some(Add),
      '-' => Some(Sub),
      '*' => Some(Mul),
      '/' => Some(Div),
      _ => None,
    }
  }
}

pub type ExprItemList = Vec<Box<(ExprOp, ExprItem)>>;

#[derive(Clone, Debug)]
pub struct Expr {
  negated: bool,
  open: bool,
  items: ExprItemList,
}

type ExprRef = Rc<RefCell<Expr>>;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ParserExpectation {
  ExpectsValue,
  ExpectsOperator,
}

pub struct Calculator<'a> {
  pub input: &'a str,
  pub size: usize,
  pub offset: usize,
  pub expr: ExprRef,
  pub expects: ParserExpectation,
  pub negated: bool,
  pub operator: ExprOp,
}

impl<'a> Calculator<'a> {
  pub fn new(input: &str) -> Calculator {
    Calculator {
      input,
      size: input.chars().count(),
      offset: 0,
      expr: Rc::new(RefCell::new(Expr {
        negated: false,
        open: true,
        items: vec![],
      })),
      expects: ParserExpectation::ExpectsValue,
      negated: false,
      operator: ExprOp::Add,
    }
  }

  // Parsing.

  pub fn parse(&mut self) {
    while let Some(c) = self.scan_char() {
      match c {
        ' ' => {
          self.offset += 1;
        }

        _ if c.is_ascii_digit() && self.expects == ExpectsValue => self.parse_literal(),
        '-' if self.expects == ExpectsValue => {
          self.negated = !self.negated;
          self.offset += 1;
        }

        '+' | '-' | '*' | '/' => {
          self.operator = ExprOp::from_char(c).unwrap();
          self.expects = ExpectsValue;
          self.offset += 1;
          self.negated = false;
        }

        '(' if self.expects == ExpectsValue => {
          let operator = self.operator;
          let negated = self.negated;
          let expr = &mut self.opening();
          expr.borrow_mut().items.push(Box::new((
            operator,
            Parens(Rc::new(RefCell::new(Expr {
              open: true,
              negated,
              items: vec![],
            }))),
          )));

          self.operator = Add;
          self.negated = false;
          self.offset += 1;
        }
        ')' if self.expects == ExpectsOperator => {
          let expr = self.opening();
          expr.borrow_mut().open = false;

          self.negated = false;
          self.offset += 1;
        }

        _ => panic!("Invalid input! {c}"),
      }
    }
  }

  fn scan_char(&self) -> Option<char> {
    self.input.chars().nth(self.offset)
  }

  fn parse_literal(&mut self) {
    let mut s = String::new();
    while let Some(c) = self.scan_char() {
      if c.is_ascii_digit() || c == '.' {
        s.push(c);
        self.offset += 1;
      } else {
        break;
      }
    }

    let op_item = Box::new((
      self.operator,
      Literal(s.parse::<f64>().unwrap() * bool_to_f64(!self.negated)),
    ));
    self.negated = false;
    self.expects = ExpectsOperator;

    let expr = self.opening();
    expr.borrow_mut().items.push(op_item);
  }

  fn opening(&mut self) -> ExprRef {
    Self::opening_in(self.expr.clone())
  }

  fn opening_in(e: ExprRef) -> ExprRef {
    let mut current = e;

    while let Some(child_pair) = current.clone().borrow().items.last() {
      let child = &child_pair.1;

      match child {
        Literal(_) => {
          break;
        }
        Parens(child_expr_ref) => {
          let child_expr = child_expr_ref.borrow();
          if child_expr.open {
            current = child_expr_ref.clone();
          } else {
            break;
          }
        }
      }
    }

    current
  }

  // Evaluation.

  pub fn eval(&mut self) -> f64 {
    println!("{:?}", self.expr.borrow_mut().items);
    Self::eval_items(&mut self.expr.borrow_mut().items)
  }

  fn eval_items(items: &mut ExprItemList) -> f64 {
    // First pass: mul/div.
    combine_adjacent(items, |mut a, mut b| {
      let left_op = a.0;
      let left_expr = &mut a.deref_mut().1;
      let right_op = b.0;
      let right_expr = &mut b.deref_mut().1;

      match right_op {
        Add | Sub => None,
        Mul => {
          let left_value: f64 = Self::eval_item(left_expr);
          let right_value: f64 = Self::eval_item(right_expr);
          Some(Box::new((left_op, Literal(left_value * right_value))))
        }
        Div => {
          let left_value: f64 = Self::eval_item(left_expr);
          let right_value: f64 = Self::eval_item(right_expr);
          Some(Box::new((left_op, Literal(left_value / right_value))))
        }
      }
    });

    // Second pass: add/sub.
    let mut result = 0f64;
    for item in items {
      match item.0 {
        Add => {
          result += Self::eval_item(&mut item.1);
        }
        Sub => {
          result -= Self::eval_item(&mut item.1);
        }
        Mul | Div => panic!("Impossible!"),
      }
    }
    result
  }

  fn eval_item(item: &mut ExprItem) -> f64 {
    match item {
      Literal(v) => *v,
      Parens(expr) => {
        let mut expr = expr.deref().borrow_mut();
        Self::eval_items(&mut expr.items) * bool_to_f64(!expr.negated)
      }
    }
  }
}

pub fn calc(input: &str) -> f64 {
  let mut calculator = Calculator::new(input);
  calculator.parse();
  calculator.eval()
}

// Utils.

fn combine_adjacent<T: Clone, F>(xs: &mut Vec<T>, f: F)
where
  F: Fn(T, T) -> Option<T>,
{
  let mut i = 1;
  while i < xs.len() {
    match f(xs[i - 1].clone(), xs[i].clone()) {
      Some(result) => {
        xs[i] = result;
        xs.remove(i - 1);
      }
      None => {
        i += 1;
      }
    }
  }
}

fn bool_to_f64(b: bool) -> f64 {
  if b {
    1f64
  } else {
    -1f64
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_approx(input: &str, expected: f64) {
    let result = calc(input);
    assert!(
      (result - expected).abs() < f64::EPSILON,
      "Expected {expected}, instead got {result}."
    );
  }

  #[test]
  fn simple_expressions() {
    assert_approx("1 + 3 - 2", 2.0);
    assert_approx("10 + 3 * 2", 16.0);
    assert_approx("2 * 3 + 6 / 2", 9.0);
  }

  #[test]
  fn nested_expressions() {
    assert_approx("10 - (4 + 2)", 4.0);
    assert_approx("2 * (3 + 1)", 8.0);
    assert_approx("3 * (6 - (2 * 2))", 6.0);
  }

  #[test]
  fn unary_minus() {
    assert_approx("5 * -1 + -(2 + 2)", -9.0);
    assert_approx("5 - -(-1)", 4.0);
  }
}
