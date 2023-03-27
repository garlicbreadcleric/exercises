use once_cell::sync::Lazy;

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

pub type Sentence = Vec<Symbol>;

#[derive(Clone)]
pub struct Rule {
  pub left: Vec<Symbol>,
  pub right: Vec<Symbol>,
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
  pub before: String,
  pub after: String,
}

#[derive(Clone)]
pub struct Grammar {
  pub rules: Vec<Rule>,
  pub initial: NonTerminal,
}

impl Grammar {
  fn new(initial: NonTerminal, rules: Vec<Rule>) -> Grammar {
    Grammar { initial, rules }
  }
}

pub struct BreadthFirstGrammarMachine {
  pub grammar: Grammar,
  pub queue: Vec<Sentence>,
  pub results: Vec<Sentence>,
  pub log: Vec<GrammarLog>,
}

impl BreadthFirstGrammarMachine {
  pub fn new(grammar: Grammar) -> BreadthFirstGrammarMachine {
    let queue = vec![vec![Symbol::from_char(grammar.initial)]];
    BreadthFirstGrammarMachine { grammar, queue, results: vec![], log: vec![] }
  }

  #[allow(clippy::should_implement_trait)]
  pub fn next(&mut self) -> bool {
    self.queue.dedup();

    let sentence = if let Some(s) = self.queue.first().cloned() {
      self.queue.remove(0);
      s
    } else {
      return false;
    };

    if sentence.iter().all(|s| s.is_terminal()) {
      self.results.push(sentence);
      return true;
    }

    let subranges = Self::subranges(&sentence);

    for subrange in subranges {
      let subsentence = sentence[subrange.0..subrange.1].to_vec();
      for rule in &self.grammar.rules {
        if rule.left == subsentence {
          let mut new_sentence = vec![];
          for &s in &sentence[..subrange.0] {
            new_sentence.push(s);
          }
          for &s in &rule.right {
            new_sentence.push(s);
          }
          for &s in &sentence[subrange.1..] {
            new_sentence.push(s);
          }

          self.queue.push(new_sentence);
        }
      }
    }

    true
  }

  fn subranges(v: &[Symbol]) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let n = v.len();
    for i in 0..n {
      for j in i + 1..=n {
        // subvectors.push(v[i..j].to_vec());
        result.push((i, j));
      }
    }
    result
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
pub static GRAMMAR: Lazy<Grammar> = Lazy::new(|| {
  Grammar::new(
    'S',
    vec![
      Rule::parse("S", "BTE"),
      Rule::parse("T", "TXa"),
      Rule::parse("T", "TXb"),
      Rule::parse("T", "TXc"),
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
      Rule::parse("aYE", "Ea"),
      Rule::parse("bYE", "Eb"),
      Rule::parse("cYE", "Ec"),
      Rule::parse("B", ""),
      Rule::parse("E", ""),
    ],
  )
});
