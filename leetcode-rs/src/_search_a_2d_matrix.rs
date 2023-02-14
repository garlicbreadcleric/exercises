// https://leetcode.com/problems/search-a-2d-matrix/

pub struct Solution;

impl Solution {
  pub fn search_matrix(matrix: Vec<Vec<i32>>, target: i32) -> bool {
    if let Some(row) = Self::search_row(&matrix, target) {
      Self::search(row, target)
    } else {
      false
    }
  }

  pub fn search_row(matrix: &[Vec<i32>], target: i32) -> Option<&[i32]> {
    let mut left_bound = 0;
    let mut right_bound = matrix.len() - 1;

    if *matrix.first()?.first()? > target {
      return None;
    }
    if *matrix.last()?.last()? < target {
      return None;
    }

    while left_bound <= right_bound {
      let i = (right_bound + left_bound) / 2;
      let row = &matrix[i];
      if *row.first()? > target {
        right_bound = i - 1;
      } else if *row.last()? < target {
        left_bound = i + 1;
      } else {
        return Some(row);
      }
    }

    None
  }

  pub fn search(vec: &[i32], target: i32) -> bool {
    let mut left_bound = 0;
    let mut right_bound = vec.len();

    while left_bound <= right_bound {
      let i = (right_bound + left_bound) / 2;
      let x = vec[i];
      if x > target {
        right_bound = i - 1;
      } else if x < target {
        left_bound = i + 1;
      } else {
        return true;
      }
    }

    false
  }
}

#[cfg(test)]
mod tests {
  use super::Solution;

  #[test]
  pub fn search_matrix_test() {
    let result1 = Solution::search_matrix(
      vec![vec![1, 3, 5, 7], vec![10, 11, 16, 20], vec![23, 30, 34, 60]],
      3,
    );
    assert!(result1);
  }
}
