// https://leetcode.com/problems/binary-tree-maximum-path-sum

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
  pub fn max_path_sum(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
    Self::max_path_sums(root)
      .map(|(max_absolute, _max_connected)| max_absolute)
      .unwrap_or(0)
  }

  pub fn max_path_sums(node: Option<Rc<RefCell<TreeNode>>>) -> Option<(i32, i32)> {
    let node = node?;

    let val = node.borrow().val;
    let left = Self::max_path_sums(node.borrow().left.clone());
    let right = Self::max_path_sums(node.borrow().right.clone());

    let (ma_left, mc_left) = left.unwrap_or((0, 0));
    let (ma_right, mc_right) = right.unwrap_or((0, 0));

    let mc = max(0, max(mc_left, mc_right)) + val;
    let mut ma = val + max(mc_left, 0) + max(mc_right, 0);
    if left.is_some() {
      ma = max(ma, ma_left);
    }
    if right.is_some() {
      ma = max(ma, ma_right);
    }
    Some((ma, mc))
  }
}
