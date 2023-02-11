// https://www.codewars.com/kata/52fe629e48970ad2bd0007e6
// FIXME: Not fast enough, fails on a timeout.

import assert from "assert";

function modpow(x, y, n) {
  if (y === 0) {
    return Math.pow(x, y) % n;
  }

  let z = 1;
  let m = x % n;

  while (z < y) {
    const gcd = fastGcd(z, y);
    if (gcd === 1) {
      break;
    }
    m = naivePowerMod(m, gcd, n);
    z *= gcd;
  }

  while (z < y) {
    m = (m * x) % n;
    z += 1;
  }

  return m;
}

function naivePowerMod(x, y, n) {
  let m = 1;
  for (let i = 0; i < y; i++) {
    m = (m * x) % n;
  }
  return m;
}

function fastGcd(x, y) {
  while (y !== 0) {
    [x, y] = [y, x % y];
  }
  return x;
}

assert.strictEqual(modpow(2, 3, 5), 3)
assert.strictEqual(modpow(4, 12, 3), 1)
assert.strictEqual(modpow(11, 10, 300), 1)
assert.strictEqual(modpow(11, 100000, 49), 32)
assert.strictEqual(modpow(5, 100000000, 19), 5)