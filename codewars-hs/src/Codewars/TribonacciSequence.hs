-- https://www.codewars.com/kata/556deca17c58da83c00002db

module Codewars.TribonacciSequence where

tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci = tribonacciAcc [] where
  tribonacciAcc xs (acc1, acc2, acc3) n =
    if n == 0
      then xs
      else tribonacciAcc (xs <> [acc1]) (acc2, acc3, acc1 + acc2 + acc3) (n - 1)
