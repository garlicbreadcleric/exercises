// https://leetcode.com/problems/balanced-binary-tree

use std::cell::RefCell;
use std::cmp::max;
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
  pub fn is_balanced(root: Option<Rc<RefCell<TreeNode>>>) -> bool {
    Self::is_balanced_with_depth(root).is_some()
  }

  fn is_balanced_with_depth(node: Option<Rc<RefCell<TreeNode>>>) -> Option<usize> {
    match node {
      None => Some(0),
      Some(node) => {
        let left = Self::is_balanced_with_depth(node.borrow().left.clone());
        let right = Self::is_balanced_with_depth(node.borrow().right.clone());

        match (left, right) {
          (Some(left_depth), Some(right_depth)) if Self::abs_diff(left_depth, right_depth) < 2 => {
            Some(max(left_depth, right_depth) + 1)
          }
          _ => None,
        }
      }
    }
  }

  // Because LeetCode doesn't support int_abs_diff yet.
  fn abs_diff(x: usize, y: usize) -> usize {
    if x > y {
      x - y
    } else {
      y - x
    }
  }
}
