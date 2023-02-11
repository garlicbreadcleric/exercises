// https://leetcode.com/problems/add-two-numbers

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct ListNode {
  pub val: i32,
  pub next: Option<Box<ListNode>>,
}

impl ListNode {
  #[inline]
  pub fn new(val: i32) -> Self {
    ListNode { next: None, val }
  }

  pub fn from_vec(vec: Vec<i32>) -> Option<Box<Self>> {
    if vec.is_empty() {
      None
    } else {
      let mut node = None;
      let mut current = &mut node;

      for x in vec {
        let next_node = Some(Box::new(ListNode { val: x, next: None }));
        match current {
          None => {
            node = next_node;
            current = &mut node;
          }
          Some(current_node) => {
            current_node.next = next_node;
            current = &mut current_node.next;
          }
        }
      }

      node
    }
  }
}

pub struct Solution;

impl Solution {
  pub fn add_two_numbers(
    mut l1: Option<Box<ListNode>>,
    mut l2: Option<Box<ListNode>>,
  ) -> Option<Box<ListNode>> {
    let mut result = None;
    let mut tail = &mut result;

    let mut total = 0;

    while l1.is_some() || l2.is_some() || total > 0 {
      let val1 = l1.as_ref().map(|node| node.val).unwrap_or(0);
      let val2 = l2.as_ref().map(|node| node.val).unwrap_or(0);

      total += val1 + val2;
      let val_next = total % 10;
      total /= 10;

      let next_node = Some(Box::new(ListNode {
        val: val_next,
        next: None,
      }));
      match tail {
        None => {
          result = next_node;
          tail = &mut result;
        }
        Some(current_node) => {
          current_node.next = next_node;
          tail = &mut current_node.next;
        }
      }
      l1 = l1.and_then(|node| node.next);
      l2 = l2.and_then(|node| node.next);
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  pub fn assert_two_sum(l1_vec: Vec<i32>, l2_vec: Vec<i32>, expected_result_vec: Vec<i32>) {
    let l1 = ListNode::from_vec(l1_vec);
    let l2 = ListNode::from_vec(l2_vec);
    let expected_result = ListNode::from_vec(expected_result_vec);
    let result = Solution::add_two_numbers(l1, l2);
    assert_eq!(result, expected_result);
  }

  #[test]
  pub fn linked_list_test() {
    let l1 = ListNode::from_vec(vec![1, 2, 3]);
    assert_eq!(l1.clone().unwrap().val, 1);
    assert_eq!(l1.clone().unwrap().next.unwrap().val, 2);
    assert_eq!(l1.clone().unwrap().next.unwrap().next.unwrap().val, 3);
    assert_eq!(l1.unwrap().next.unwrap().next.unwrap().next, None);
  }

  #[test]
  pub fn two_sum_test() {
    assert_two_sum(vec![2, 4, 3], vec![5, 6, 4], vec![7, 0, 8]);
    assert_two_sum(vec![0], vec![0], vec![0]);
    assert_two_sum(
      vec![9, 9, 9, 9, 9, 9, 9],
      vec![9, 9, 9, 9],
      vec![8, 9, 9, 9, 0, 0, 0, 1],
    );
  }
}
