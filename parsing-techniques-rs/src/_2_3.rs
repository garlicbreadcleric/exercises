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
  left: NonTerminal,
  right: Vec<Symbol>,
}

impl Rule {
  pub fn new(left: char, right: Vec<Symbol>) -> Rule {
    Rule { left, right }
  }

  pub fn parse(left: char, right: &str) -> Rule {
    Rule { left, right: right.chars().map(Symbol::from_char).collect() }
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

pub struct GrammarMachine {
  grammar: Grammar,
  symbols: Vec<Symbol>,
  log: Vec<GrammarLog>,
}

impl GrammarMachine {
  pub fn new(grammar: Grammar) -> GrammarMachine {
    let mut symbols = vec![];
    for c in &grammar.initials {
      symbols.push(Symbol::from_char(*c));
    }
    GrammarMachine { grammar, symbols, log: vec![] }
  }

  pub fn apply_random_rule(&mut self) {
    let mut rng = rand::thread_rng();

    let rule_index = rng.gen_range(0..self.grammar.rules.len());
    let rule = &self.grammar.rules[rule_index];

    let mut symbols = vec![];

    for s in &self.symbols {
      if *s == Symbol::from_char(rule.left) {
        for s2 in &rule.right {
          symbols.push(*s2);
        }
      } else {
        symbols.push(*s);
      }
    }

    let before = self.symbols_to_string();
    self.symbols = symbols;
    let after = self.symbols_to_string();

    self.log.push(GrammarLog { before, after });
  }

  fn symbols_to_string(&self) -> String {
    self.symbols.iter().map(|s| s.to_char()).collect()
  }
}

/// Grammar for a Manhattan turtle that is not allowed to the west of it's starting point.
///
/// ```text
/// S -> SeSwS
/// S -> SeS | SnS | SsS
/// S -> ϵ
/// ```
static GRAMMAR: Lazy<Grammar> = Lazy::new(|| {
  Grammar::new(
    vec!['S'],
    vec![
      Rule::parse('S', "SeSwS"),
      Rule::parse('S', "SeS"),
      Rule::parse('S', "SnS"),
      Rule::parse('S', "SsS"),
      Rule::parse('S', ""),
    ],
  )
});

#[rustfmt::skip]
pub fn is_valid_manhattan_turtle_path(symbols: &[Symbol]) -> bool {
  let mut position = (0, 0);
  for s in symbols {
    if let Symbol::Terminal(c) = s {
      match c {
        'e' => { position.0 += 1; },
        'w' => { position.0 -= 1; },
        'n' => { position.1 += 1; },
        's' => { position.1 -= 1; },
        _ => { return false; },
      }
    }

    if position.0 < 0 { return false; }
  }
  true
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn manhattan_turtle_test() {
    for _ in 0..10 {
      let mut gm = GrammarMachine::new(GRAMMAR.clone());

      for _ in 0..1000 {
        gm.apply_random_rule();
        assert!(is_valid_manhattan_turtle_path(&gm.symbols), "{:?}", gm.log);
      }
    }
  }
}
