// https://leetcode.com/problems/median-of-two-sorted-arrays

pub struct Solution;

impl Solution {
  pub fn find_median_sorted_arrays(nums1: Vec<i32>, nums2: Vec<i32>) -> f64 {
    let mut i = 0;
    let mut j = 0;
    let mut x = 0f64;
    let mut y = 0f64;

    while i + j <= (nums1.len() + nums2.len()) / 2 {
      if i < nums1.len() && (j >= nums2.len() || nums1[i] <= nums2[j]) {
        x = y;
        y = nums1[i] as f64;
        i += 1;
      } else if j < nums2.len() && (i >= nums1.len() || nums1[i] >= nums2[j]) {
        x = y;
        y = nums2[j] as f64;
        j += 1;
      } else {
        panic!("Impossible!");
      }
    }

    if (nums1.len() + nums2.len()) % 2 == 0 {
      (x + y) / 2f64
    } else {
      y
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_approx(nums1: Vec<i32>, nums2: Vec<i32>, expected_result: f64) {
    let result = Solution::find_median_sorted_arrays(nums1, nums2);
    assert!(
      (result - expected_result).abs() < f64::EPSILON,
      "Expected {expected_result}, instead got {result}."
    );
  }

  #[test]
  pub fn median_test() {
    assert_approx(vec![1, 3, 4, 7, 9, 10, 12], vec![5, 6, 13, 14], 7f64);
    assert_approx(vec![1, 3, 4, 5, 9, 14, 15, 20], vec![2, 6, 16, 17], 7.5f64);
    assert_approx(vec![1, 3], vec![2], 2f64);
    assert_approx(vec![1, 2], vec![3, 4], 2.5f64);
    assert_approx(vec![1, 3], vec![2, 4], 2.5f64);
  }
}
