// https://leetcode.com/problems/symmetric-tree

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
  pub fn is_symmetric(root: Option<Rc<RefCell<TreeNode>>>) -> bool {
    match root {
      None => true,
      Some(node) => Self::are_symmetric(node.borrow().left.clone(), node.borrow().right.clone()),
    }
  }

  pub fn are_symmetric(p: Option<Rc<RefCell<TreeNode>>>, q: Option<Rc<RefCell<TreeNode>>>) -> bool {
    match (p, q) {
      (None, None) => true,
      (None, Some(_)) => false,
      (Some(_), None) => false,
      (Some(p), Some(q)) => {
        p.borrow().val == q.borrow().val
          && Self::are_symmetric(p.borrow().left.clone(), q.borrow().right.clone())
          && Self::are_symmetric(p.borrow().right.clone(), q.borrow().left.clone())
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn is_symmetric_test() {
    let tree1 = Some(Rc::new(RefCell::new(TreeNode {
      val: 1,
      left: Some(Rc::new(RefCell::new(TreeNode {
        val: 2,
        left: Some(Rc::new(RefCell::new(TreeNode {
          val: 3,
          left: None,
          right: None,
        }))),
        right: None,
      }))),
      right: Some(Rc::new(RefCell::new(TreeNode {
        val: 2,
        left: None,
        right: Some(Rc::new(RefCell::new(TreeNode {
          val: 3,
          left: None,
          right: None,
        }))),
      }))),
    })));

    assert!(Solution::is_symmetric(tree1));
  }
}
