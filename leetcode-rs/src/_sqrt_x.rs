// https://leetcode.com/problems/sqrtx/

pub struct Solution;

impl Solution {
  pub fn my_sqrt(x: i32) -> i32 {
    let x = x as i64;
    let mut lb = 0;
    let mut rb = x;

    loop {
      let y = (lb + rb) / 2;
      let y_sqr = y * y;
      let y1_sqr = (y + 1) * (y + 1);

      if y_sqr <= x && y1_sqr > x {
        return y as i32;
      }

      if y1_sqr <= x {
        lb = y + 1;
      } else if y_sqr > x {
        rb = y - 1;
      }
    }
  }
}

#[cfg(test)]
mod tests {
  use super::Solution;

  #[test]
  pub fn sqrt_tests() {
    assert_eq!(Solution::my_sqrt(4), 2);
    assert_eq!(Solution::my_sqrt(5), 2);
    assert_eq!(Solution::my_sqrt(8), 2);
    assert_eq!(Solution::my_sqrt(9), 3);
    assert_eq!(Solution::my_sqrt(15), 3);
    assert_eq!(Solution::my_sqrt(16), 4);
    assert_eq!(Solution::my_sqrt(2147395599), 46339);
  }
}
