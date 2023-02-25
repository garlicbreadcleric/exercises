// https://leetcode.com/problems/maximum-gap

pub struct Solution;

#[derive(Copy, Clone, Debug)]
pub struct Bucket { pub min: i32, pub max: i32 }

impl Solution {
  pub fn maximum_gap(xs: Vec<i32>) -> i32 {
    if xs.len() < 2 { return 0; }

    let x_min = *xs.iter().min().unwrap();
    let x_max = *xs.iter().max().unwrap();

    let bucket_size = ((x_max - x_min) / (xs.len() as i32 - 1)).max(1) as usize;
    let bucket_count = {
      let mut bucket_count = ((x_max - x_min) as usize) / bucket_size;
      if bucket_count * bucket_size <= (x_max - x_min) as usize {
        bucket_count += 1;
      }
      bucket_count
    };

    let mut buckets: Vec<Option<Bucket>> = vec![None; bucket_count];

    for &x in &xs {
      let bucket_index = ((x - x_min) / bucket_size as i32) as usize;

      if let Some(bucket) = &mut buckets[bucket_index] {
        if x < bucket.min {
          bucket.min = x;
        }
        if x > bucket.max {
          bucket.max = x;
        }
      } else {
        buckets[bucket_index] = Some(Bucket { min: x, max: x });
      }
    }

    let mut last_max = buckets[0].unwrap().min;
    let mut max_diff = 0;

    for bucket in buckets.iter().flatten() {
      if bucket.min - last_max > max_diff {
        max_diff = bucket.min - last_max;
      }
      last_max = bucket.max;
    }

    max_diff
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  pub fn maximum_gap_test() {
    assert_eq!(Solution::maximum_gap(vec![1, 1, 1, 1]), 0);
    assert_eq!(Solution::maximum_gap(vec![1, 1, 2, 1]), 1);
    assert_eq!(Solution::maximum_gap(vec![1,1,1,1,1,5,5,5,5,5]), 4);
    assert_eq!(Solution::maximum_gap(vec![3, 6, 9, 1]), 3);
    assert_eq!(Solution::maximum_gap(vec![3, 1, 10, 2, 7, 15, 6]), 5);
    assert_eq!(Solution::maximum_gap(vec![38, 20, 52, 27, 19, 11, 41, 3]), 11);
    assert_eq!(Solution::maximum_gap(vec![10]), 0);
    assert_eq!(
      Solution::maximum_gap(vec![
        494767408, 862126209, 213511142, 768240025, 631263193, 839199222, 990848796, 214568815,
        540853864, 760724418, 980162605, 976743981, 168965760, 680875496, 256709197, 970953816,
        948680062, 347254784, 732201399, 786249847, 782805044, 40906641, 674241381, 784330934,
        175502610, 731105392, 424650824, 549764101, 986090032, 710542549, 249208107, 448419816,
        527708664, 158499246, 223421723, 114552457, 466978424, 821491411, 19614107, 115389497,
        902211438, 2644108, 744489871, 897895073, 372311214, 551142753, 933294894, 426217662,
        217504874, 983488406, 516890023, 426853110, 971124103
      ]),
      90545587
    );
  }
}
