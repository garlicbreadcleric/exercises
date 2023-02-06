-- https://www.codewars.com/kata/582c5382f000e535100001a7

module Codewars.ParseALinkedListFromString where

parse :: String -> [Word]
parse = map read . filter (/= "->") . filter (/= "null") . words
