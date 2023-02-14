// https://leetcode.com/problems/search-a-2d-matrix-ii

pub struct Solution;

impl Solution {
  pub fn search_matrix(matrix: Vec<Vec<i32>>, target: i32) -> bool {
    for row in matrix {
      let mut lower = 0;
      let mut upper = row.len() - 1;

      while lower <= upper {
        let i = (lower + upper) / 2;
        let x = row[i];

        if target > x {
          lower = i + 1;
        } else if target < x {
          if let Some(new_upper) = i.checked_sub(1) {
            upper = new_upper;
          } else {
            return false;
          }
        } else {
          return true;
        }
      }
    }

    false
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn search_matrix_test() {
    let result1 = Solution::search_matrix(
      vec![
        vec![1, 4, 7, 11, 15],
        vec![2, 5, 8, 12, 19],
        vec![3, 6, 9, 16, 22],
        vec![10, 13, 14, 17, 24],
        vec![18, 21, 23, 26, 30],
      ],
      5,
    );
    assert!(result1);

    let result2 = Solution::search_matrix(
      vec![
        vec![1, 4, 7, 11, 15],
        vec![2, 5, 8, 12, 19],
        vec![3, 6, 9, 16, 22],
        vec![10, 13, 14, 17, 24],
        vec![18, 21, 23, 26, 30],
      ],
      20,
    );
    assert!(!result2);

    let result3 = Solution::search_matrix(vec![vec![1, 3]], 3);
    assert!(result3);

    let result4 = Solution::search_matrix(vec![vec![-1, 3]], -1);
    assert!(result4);

    let result5 = Solution::search_matrix(
      vec![
        vec![1, 2, 3, 4, 5],
        vec![6, 7, 8, 9, 10],
        vec![11, 12, 13, 14, 15],
        vec![16, 17, 18, 19, 20],
        vec![21, 22, 23, 24, 25],
      ],
      15,
    );
    assert!(result5)
  }
}
