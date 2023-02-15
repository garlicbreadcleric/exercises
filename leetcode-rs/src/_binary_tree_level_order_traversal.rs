use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Eq)]
pub struct TreeNode {
  pub val: i32,
  pub left: Option<Rc<RefCell<TreeNode>>>,
  pub right: Option<Rc<RefCell<TreeNode>>>,
}

impl TreeNode {
  #[inline]
  pub fn new(val: i32) -> Self {
    TreeNode {
      val,
      left: None,
      right: None,
    }
  }
}

pub struct Solution;

impl Solution {
  pub fn level_order(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<Vec<i32>> {
    let mut result = vec![];
    Self::level_order_mut(root, &mut result, 0);
    result
  }

  pub fn level_order_mut(
    node: Option<Rc<RefCell<TreeNode>>>,
    values: &mut Vec<Vec<i32>>,
    level: usize,
  ) {
    if let Some(node) = node {
      while level >= values.len() {
        values.push(vec![]);
      }
      values[level].push(node.borrow().val);
      Self::level_order_mut(node.borrow().left.clone(), values, level + 1);
      Self::level_order_mut(node.borrow().right.clone(), values, level + 1);
    }
  }
}
