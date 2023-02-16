// https://leetcode.com/problems/plus-one

pub struct Solution;

impl Solution {
  pub fn plus_one(digits: Vec<i32>) -> Vec<i32> {
    let mut result = Vec::with_capacity(digits.len() + 1);

    let mut overflow = true;
    for digit in digits.iter().rev() {
      let digit = if overflow { *digit + 1 } else { *digit };
      let digit = if digit > 9 {
        overflow = true;
        digit % 10
      } else {
        overflow = false;
        digit
      };
      result.insert(0, digit);
    }

    if overflow {
      result.insert(0, 1);
    }

    result
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn plus_one_test() {
    assert_eq!(Solution::plus_one(vec![1, 2, 3]), vec![1, 2, 4]);
    assert_eq!(Solution::plus_one(vec![4, 3, 2, 1]), vec![4, 3, 2, 2]);
    assert_eq!(Solution::plus_one(vec![9]), vec![1, 0]);
  }
}
