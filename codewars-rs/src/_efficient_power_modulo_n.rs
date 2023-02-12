// https://www.codewars.com/kata/52fe629e48970ad2bd0007e6

pub fn power_mod(x: u64, y: u64, n: u64) -> u64 {
  let mut m = 1 % n;
  for i in 0..64u64 {
    if y & (1u64 << i) != 0 {
      let mut k = x % n;
      for _ in 0..i {
        k = (k * k) % n;
      }
      m = m * k % n;
    }
  }
  m
}

#[cfg(test)]
mod tests {
  use super::power_mod;

  #[test]
  pub fn power_mod_test() {
    assert_eq!(power_mod(2, 3, 3), 2);
    assert_eq!(power_mod(3, 2, 4), 1);
    assert_eq!(power_mod(3, 3, 5), 2);
    assert_eq!(power_mod(2, 3, 5), 3);
    assert_eq!(power_mod(4, 12, 3), 1);
    assert_eq!(power_mod(11, 10, 300), 1);
    assert_eq!(power_mod(11, 100000, 49), 32);
    assert_eq!(power_mod(5, 100000000, 19), 5);
  }
}
