// https://leetcode.com/problems/unique-paths

pub struct Solution;

impl Solution {
  pub fn unique_paths(m: i32, n: i32) -> i32 {
    let mut r = vec![0; m as usize];
    r[m as usize - 1] = 1;

    for _ in 1..n {
      for i in 0..(m as usize) {
        r[i] = r.iter().skip(i).sum();
      }
    }

    r.iter().sum()
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn unique_paths_test() {
    assert_eq!(Solution::unique_paths(3, 7), 28);
    assert_eq!(Solution::unique_paths(3, 2), 3);
    assert_eq!(Solution::unique_paths(2, 1), 1);
  }
}
