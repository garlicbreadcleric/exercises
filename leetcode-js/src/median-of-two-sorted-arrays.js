// https://leetcode.com/problems/median-of-two-sorted-arrays

import assert from "assert";

/**
 * @param {number[]} nums1
 * @param {number[]} nums2
 * @return {number}
 */
function findMedianSortedArrays(nums1, nums2) {
  if (nums1.length > nums2.length) {
    [nums1, nums2] = [nums2, nums1];
  }

  let iMin = 0;
  let iMax = nums1.length - 1;

  let i, j;

  const half = Math.floor((nums1.length + nums2.length) / 2);

  while (true) {
    i = Math.floor((iMin + iMax) / 2);
    j = half - i - 2;

    if (i >= 0 && i < nums1.length && j + 1 < nums2.length && nums1[i] > nums2[j + 1]) {
      iMax = i - 1;
    } else if (j >= 0 && i + 1 < nums1.length && j < nums2.length && nums1[i + 1] < nums2[j]) {
      iMin = i + 1;
    } else {
      break;
    }
  }

  console.log(i, iMin, iMax);

  if ((nums1.length + nums2.length) % 2 === 0) {
    return average(max(nums1[i], nums2[j]), min(nums1[i + 1], nums2[j + 1]));
  }

  return min(nums1[i + 1], nums2[j + 1]);
}

function max(x, y) {
  if (x == null) return y;
  if (y == null) return x;
  return Math.max(x, y);
}

function min(x, y) {
  if (x == null) return y;
  if (y == null) return x;
  return Math.min(x, y);
}

function average(x, y) {
  if (x == null) return y;
  if (y == null) return x;
  return (x + y) / 2;
}

function findMedian(nums) {
  return (nums[Math.floor((nums.length - 1) / 2)] + nums[Math.ceil((nums.length - 1) / 2)]) / 2;
}

// prettier-ignore
const arrayPairs = [
  [[1, 3, 4, 6, 8, 10, 11, 20], [2, 5, 7, 12, 13, 14, 21]],
  [[1, 5, 6, 7, 10],            [3, 4]],
  [[5, 6, 7, 10, 11],           [1, 2, 3]],
  [[5],                         [1, 2, 3]],
  [[6],                         [1, 2]],
  [[5, 6, 7, 8],                [1]],
  [[6, 7, 8, 9],                [2, 3]],
  [[2],                         []]
];

for (const [nums1, nums2] of arrayPairs) {
  const nums = nums1.concat(nums2);
  nums.sort((a, b) => a - b);
  const expectedResult = findMedian(nums);
  const result = findMedianSortedArrays(nums1, nums2);
  assert(Math.abs(expectedResult - result) < 0.001, `Expected ${expectedResult}, instead got ${result}.\n${JSON.stringify(nums1)} ${JSON.stringify(nums2)}`);
}

console.log(
  findMedianSortedArrays([1, 3, 4, 5, 9, 14, 15, 20], [2, 6, 16, 17])
)
