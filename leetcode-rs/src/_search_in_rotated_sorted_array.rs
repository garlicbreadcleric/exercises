// https://leetcode.com/problems/search-in-rotated-sorted-array

pub struct Solution;

impl Solution {
  pub fn search(nums: Vec<i32>, target: i32) -> i32 {
    let pivot = Self::find_pivot(&nums);
    let mut i_min = 0;
    let mut i_max = nums.len() - 1;

    while i_min <= i_max {
      let i = (i_min + i_max) / 2;
      let i_rotated = (i + pivot) % nums.len();
      let x = nums[i_rotated];

      if x == target {
        return i_rotated as i32;
      } else if x > target {
        if let Some(i_max_new) = i.checked_sub(1) {
          i_max = i_max_new;
        } else {
          break;
        }
      } else if x < target {
        i_min = i + 1;
      }
    }

    return -1;
  }

  pub fn find_pivot(nums: &[i32]) -> usize {
    let mut i_min = 0;
    let mut i_max = nums.len() - 1;
    let mut x_min = nums[0];
    let mut x_max = *nums.last().unwrap();

    while i_min <= i_max {
      let i = (i_min + i_max) / 2;
      let x = nums[i];

      if i > 0 && nums[i - 1] > x {
        return i;
      }

      if x_min > x {
        i_max = i - 1;
        x_max = x;
      } else if x_max < x {
        i_min = i + 1;
        x_min = x;
      } else {
        break;
      }
    }

    0
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn find_pivot_test() {
    assert_eq!(Solution::find_pivot(&[1]), 0);
    assert_eq!(Solution::find_pivot(&[1, 3, 5]), 0);
    assert_eq!(Solution::find_pivot(&[0, 1, 2, 3, 4, 5]), 0);
    assert_eq!(Solution::find_pivot(&[2, 3, 4, 0, 1]), 3);
    assert_eq!(Solution::find_pivot(&[2, 3, 4, 5, 0, 1]), 4);
    assert_eq!(
      Solution::find_pivot(&[5, 6, 10, 12, 20, 25, 30, 31, 32, 2, 3, 4]),
      9
    );
  }

  #[test]
  pub fn search_test() {
    assert_eq!(Solution::search(vec![1], 2), -1);
    assert_eq!(Solution::search(vec![1, 3, 5], 5), 2);
  }
}
