// https://leetcode.com/problems/count-sorted-vowel-strings

/**
 * @param {number} n
 * @return {number}
 */
function countVowelStrings(n) {
  let cs = [1, 1, 1, 1, 1];
  for (let i = 1; i < n; i++) {
    for (j = 0; j < 5; j++) {
      let csj = 0;
      for (let k = j; k < 5; k++) { csj += cs[k]; }
      cs[j] = csj;
    }
  }

  return cs.reduce((a, b) => a + b);
};