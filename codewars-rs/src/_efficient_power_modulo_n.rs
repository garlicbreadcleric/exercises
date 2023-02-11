// https://www.codewars.com/kata/52fe629e48970ad2bd0007e6
// FIXME: Not fast enough, fails on a timeout.

pub fn power_mod(x: u64, y: u64, n: u64) -> u64 {
  if y == 0 {
    return x.pow(y as u32) % n;
  }

  let mut z = 1;
  let mut m = x % n;

  while z < y {
    let gcd = fast_gcd(z, y);
    if gcd == 1 {
      break;
    }
    m = naive_power_mod(m, gcd, n);
    z *= gcd;
  }

  while z < y {
    m = (m * x) % n;
    z += 1;
  }

  m
}

fn naive_power_mod(x: u64, y: u64, n: u64) -> u64 {
  let mut m = 1;
  for _ in 0..y {
    m = (m * x) % n;
  }
  m
}

fn fast_gcd(mut x: u64, mut y: u64) -> u64 {
  while y != 0 {
    (x, y) = (y, x % y)
  }
  x
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
