// https://leetcode.com/problems/binary-tree-inorder-traversal

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
  pub fn inorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
    let mut current = root;
    let mut stack = vec![];
    let mut result = vec![];

    while !stack.is_empty() || current.is_some() {
      while let Some(node) = current {
        stack.push(node.clone());
        current = node.borrow().left.clone();
      }

      current = stack.pop();
      result.push(current.clone().unwrap().borrow().val);
      current = current.clone().unwrap().borrow().right.clone();
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn traversal_test() {
    assert_eq!(
      Solution::inorder_traversal(Some(Rc::new(RefCell::new(TreeNode {
        val: 10,
        left: Some(Rc::new(RefCell::new(TreeNode {
          val: 20,
          left: Some(Rc::new(RefCell::new(TreeNode {
            val: 30,
            left: None,
            right: None
          }))),
          right: None
        }))),
        right: Some(Rc::new(RefCell::new(TreeNode {
          val: 40,
          left: None,
          right: None
        })))
      })))),
      vec![30, 20, 10, 40]
    );

    assert_eq!(
      Solution::inorder_traversal(Some(Rc::new(RefCell::new(TreeNode {
        val: 10,
        left: Some(Rc::new(RefCell::new(TreeNode {
          val: 20,
          left: Some(Rc::new(RefCell::new(TreeNode {
            val: 30,
            left: None,
            right: Some(Rc::new(RefCell::new(TreeNode {
              val: 35,
              left: None,
              right: None
            })))
          }))),
          right: None
        }))),
        right: Some(Rc::new(RefCell::new(TreeNode {
          val: 40,
          left: None,
          right: None
        })))
      })))),
      vec![30, 35, 20, 10, 40]
    );
  }
}
