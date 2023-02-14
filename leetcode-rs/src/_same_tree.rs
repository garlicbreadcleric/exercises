// https://leetcode.com/problems/same-tree

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
  pub fn is_same_tree(p: Option<Rc<RefCell<TreeNode>>>, q: Option<Rc<RefCell<TreeNode>>>) -> bool {
    let mut current_p = p;
    let mut current_q = q;

    let mut stack = vec![];

    while !stack.is_empty() || current_p.is_some() || current_q.is_some() {
      while current_p.is_some() && current_q.is_some() {
        stack.push((current_p.clone().unwrap(), current_q.clone().unwrap()));
        current_p = current_p.clone().unwrap().borrow().left.clone();
        current_q = current_q.clone().unwrap().borrow().left.clone();
      }

      if current_p.is_some() != current_q.is_some() {
        return false;
      }

      if let Some((previous_p, previous_q)) = stack.pop() {
        if previous_p.borrow().val != previous_q.borrow().val {
          return false;
        }

        current_p = Some(previous_p);
        current_q = Some(previous_q);
      } else {
        current_p = None;
        current_q = None;
      }

      current_p = current_p.and_then(|n| n.borrow().right.clone());
      current_q = current_q.and_then(|n| n.borrow().right.clone());
    }

    true
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn same_tree_test() {
    let tree1 = Some(Rc::new(RefCell::new(TreeNode {
      val: 10,
      left: Some(Rc::new(RefCell::new(TreeNode {
        val: 20,
        left: Some(Rc::new(RefCell::new(TreeNode {
          val: 30,
          left: None,
          right: None,
        }))),
        right: None,
      }))),
      right: Some(Rc::new(RefCell::new(TreeNode {
        val: 40,
        left: None,
        right: None,
      }))),
    })));

    let tree2 = Some(Rc::new(RefCell::new(TreeNode {
      val: 10,
      left: Some(Rc::new(RefCell::new(TreeNode {
        val: 20,
        left: Some(Rc::new(RefCell::new(TreeNode {
          val: 30,
          left: None,
          right: Some(Rc::new(RefCell::new(TreeNode {
            val: 35,
            left: None,
            right: None,
          }))),
        }))),
        right: None,
      }))),
      right: Some(Rc::new(RefCell::new(TreeNode {
        val: 40,
        left: None,
        right: None,
      }))),
    })));

    assert!(Solution::is_same_tree(tree1.clone(), tree1.clone()));
    assert!(Solution::is_same_tree(tree2.clone(), tree2.clone()));

    assert!(!Solution::is_same_tree(tree1, tree2));
  }
}
