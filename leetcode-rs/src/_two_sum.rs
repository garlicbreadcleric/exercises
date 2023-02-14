// https://leetcode.com/problems/two-sum

use std::collections::HashMap;

pub struct Solution;

impl Solution {
  pub fn two_sum(nums: Vec<i32>, target: i32) -> Vec<i32> {
    let mut positions = HashMap::new();
    for (i, x) in nums.iter().enumerate() {
      let y = target - x;
      if let Some(j) = positions.get(&y) {
        return vec![i as i32, *j];
      }
      positions.insert(x, i as i32);
    }

    vec![] // Unreachable.
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_sum(nums: Vec<i32>, target: i32, expected_result: Vec<i32>) {
    let mut result = Solution::two_sum(nums, target);
    result.sort();
    assert_eq!(result, expected_result);
  }

  #[test]
  pub fn two_sum_test() {
    assert_sum(vec![2, 7, 11, 15], 9, vec![0, 1]);
    assert_sum(vec![3, 2, 4], 6, vec![1, 2]);
    assert_sum(vec![3, 3], 6, vec![0, 1]);
  }
}
