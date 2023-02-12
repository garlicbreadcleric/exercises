// https://www.codewars.com/kata/52fe629e48970ad2bd0007e6

import assert from "assert";

function modpow(x, y, n) {
  let m = 1 % n;
  for (let i = 0; i < 32; i++) {
    if (y & (1 << i)) {
      let k = x % n;
      for (let j = 0; j < i; j++) {
        k = (k * k) % n;
      }
      m = m * k % n;
    }
  }
  return m;
}

assert.strictEqual(modpow(2, 3, 5), 3)
assert.strictEqual(modpow(4, 12, 3), 1)
assert.strictEqual(modpow(11, 10, 300), 1)
assert.strictEqual(modpow(11, 100000, 49), 32)
assert.strictEqual(modpow(5, 100000000, 19), 5)