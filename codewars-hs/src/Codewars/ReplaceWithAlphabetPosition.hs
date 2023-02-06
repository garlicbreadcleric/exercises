-- https://www.codewars.com/kata/546f922b54af40e1e90001da

module Codewars.ReplaceWithAlphabetPosition where

import Data.Char (toLower)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

alphabetPosition :: String -> String
alphabetPosition = unwords . concatMap charPosition
  where
    charPosition c = fromMaybe [] $ do
      position <- elemIndex (toLower c) ['a' .. 'z']
      pure [show (position + 1)]
