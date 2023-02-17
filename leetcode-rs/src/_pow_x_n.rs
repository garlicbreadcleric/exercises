// https://leetcode.com/problems/powx-n
pub struct Solution;

impl Solution {
  pub fn my_pow(x: f64, n: i32) -> f64 {
    if (x - 1f64).abs() < f64::EPSILON {
      return x;
    }

    if n == 0 {
      return 1f64;
    }
    let (x, n) = if n > 0 {
      (x, n as i64)
    } else {
      (1f64 / x, -(n as i64))
    };

    let mut r = x;
    let mut m = 1;
    while m < n {
      if r.abs() < f64::EPSILON {
        return r;
      }

      if m * 2 <= n {
        r *= r;
        m *= 2;
      } else {
        r *= x;
        m += 1;
      }
    }

    r
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  fn assert_approx(x: f64, n: i32, expected_result: f64) {
    let result = Solution::my_pow(x, n);
    assert!(
      (result - expected_result).abs() < 0.00001,
      "Expected {expected_result}, instead got {result}."
    );
  }

  #[test]
  pub fn pow_test() {
    assert_approx(3.0, 2, 9.0);
    assert_approx(2.0, 4, 16.0);
    assert_approx(2.0, 8, 256.0);
    assert_approx(2.0, 10, 1024.0);

    assert_approx(8.84372, -5, 0.00002);
    assert_approx(2.0, -2147483648, 0.0);
  }
}
