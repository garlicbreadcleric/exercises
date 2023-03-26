use once_cell::sync::Lazy;
use rand::prelude::*;

pub type NonTerminal = char;
pub type Terminal = char;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Symbol {
  NonTerminal(NonTerminal),
  Terminal(Terminal),
}

impl Symbol {
  pub fn from_char(c: char) -> Symbol {
    if c.is_uppercase() {
      Symbol::NonTerminal(c)
    } else {
      Symbol::Terminal(c)
    }
  }

  pub fn to_char(&self) -> char {
    match self {
      Symbol::NonTerminal(c) | Symbol::Terminal(c) => *c,
    }
  }

  pub fn is_terminal(&self) -> bool {
    match self {
      Symbol::NonTerminal(_) => false,
      Symbol::Terminal(_) => true,
    }
  }

  pub fn is_non_terminal(&self) -> bool {
    match self {
      Symbol::NonTerminal(_) => true,
      Symbol::Terminal(_) => false,
    }
  }
}

#[derive(Clone)]
pub struct Rule {
  left: Vec<Symbol>,
  right: Vec<Symbol>,
}

impl Rule {
  pub fn new(left: Vec<Symbol>, right: Vec<Symbol>) -> Rule {
    Rule { left, right }
  }

  pub fn parse(left: &str, right: &str) -> Rule {
    Rule { left: left.chars().map(Symbol::from_char).collect(), right: right.chars().map(Symbol::from_char).collect() }
  }
}

#[derive(Debug)]
pub struct GrammarLog {
  before: String,
  after: String,
}

#[derive(Clone)]
pub struct Grammar {
  rules: Vec<Rule>,
  initials: Vec<NonTerminal>,
}

impl Grammar {
  fn new(initials: Vec<NonTerminal>, rules: Vec<Rule>) -> Grammar {
    Grammar { initials, rules }
  }
}

/// Grammar for a language that produces strings `ww` consisting of two equal substrings `w`.
///
/// ```text
/// S -> BTE
/// T -> XTa | XTb | XTc | _
///
/// Xa -> aaY, Xb -> bbY, Xc -> ccY
///
/// aYa -> aaY, aYb -> baY, aYc -> caY
/// bYa -> abY, bYb -> bbY, bYc -> cbY
/// cYa -> acY, cYb -> bcY, cYc -> ccY
///
/// YE -> E
///
/// B -> _
/// E -> _
/// ```
static GRAMMAR: Lazy<Grammar> = Lazy::new(|| {
  Grammar::new(
    vec!['S'],
    vec![
      Rule::parse("S", "BTE"),
      Rule::parse("T", "XTa"),
      Rule::parse("T", "XTb"),
      Rule::parse("T", "XTc"),
      Rule::parse("T", ""),
      Rule::parse("Xa", "aaY"),
      Rule::parse("Xb", "bbY"),
      Rule::parse("Xc", "ccY"),
      Rule::parse("aYa", "aaY"),
      Rule::parse("aYb", "baY"),
      Rule::parse("aYc", "caY"),
      Rule::parse("bYa", "abY"),
      Rule::parse("bYb", "bbY"),
      Rule::parse("bYc", "cbY"),
      Rule::parse("cYa", "acY"),
      Rule::parse("cYb", "bcY"),
      Rule::parse("cYc", "ccY"),
      Rule::parse("YE", "E"),
      Rule::parse("B", ""),
      Rule::parse("E", ""),
    ],
  )
});
