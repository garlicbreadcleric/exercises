-- https://www.codewars.com/kata/634420abae4b81004afefca7

module Codewars.FindTheNthEvilNumber where

import Data.Bits

getEvil :: Integer -> Integer
getEvil n = if isEvil (n * 2 - 1) then n * 2 - 1 else (n - 1) * 2

isEvil :: Integer -> Bool
isEvil 0 = True
isEvil 1 = False
isEvil n = case n .&. 1 of
  0 -> isEvil (n `shiftR` 1)
  1 -> not (isEvil (n `shiftR` 1))
  _ -> error "Impossible!"
