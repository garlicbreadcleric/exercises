-- https://www.codewars.com/kata/5453af58e6c920858d000823

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Codewars.TheMostImperativeFunctionalLanguage where

import Control.Applicative (Applicative (..))
import Data.Bifunctor (Bifunctor (second))

newtype VarState a = VarState {runVarState :: [Integer] -> ([Integer], a)}

type VarGetter = [Integer] -> Maybe Integer

type VarSetter = Integer -> [Integer] -> [Integer]

data VarPtr = VarPtr {getter :: VarGetter, setter :: VarSetter}

instance Functor VarState where
  fmap f s = VarState (second f . runVarState s)

instance Applicative VarState where
  pure x = VarState (,x)
  liftA2 f sa sb = VarState \vars ->
    let (vars', a) = runVarState sa vars
        (vars'', b) = runVarState sb vars'
     in (vars'', f a b)

instance Monad VarState where
  s >>= f = VarState \vars -> let (vars', a) = runVarState s vars in runVarState (f a) vars'

def :: VarState VarPtr -> Integer
def s = snd . flip runVarState [] $ do
  p <- s
  m <- deref p
  case m of
    Just v -> pure v
    Nothing -> error "Invalid pointer exception!"

var :: Integer -> VarState VarPtr
var x = VarState \vars -> (vars <> [x], VarPtr (getAt (length vars)) (setAt (length vars)))

deref :: VarPtr -> VarState (Maybe Integer)
deref g = VarState (\vars -> (vars, getter g vars))

lit :: Integer -> VarPtr
lit x = VarPtr {getter = const (Just x), setter = const id}

while :: VarPtr -> (Integer -> Bool) -> VarState a -> VarState ()
while p f act = do
  m <- deref p
  case m of
    Just v | f v -> do
      _ <- act
      while p f act
    _ -> pure ()

modifyBinOp :: (Integer -> Integer -> Integer) -> VarPtr -> VarPtr -> VarState ()
modifyBinOp f p1 p2 = do
  m1 <- deref p1
  m2 <- deref p2
  case (m1, m2) of
    (Just v1, Just v2) -> VarState \vars -> (setter p1 (v1 `f` v2) vars, ())
    _ -> pure ()

(+=), (-=), (*=) :: VarPtr -> VarPtr -> VarState ()
(+=) = modifyBinOp (+)
(-=) = modifyBinOp (-)
(*=) = modifyBinOp (*)

-- Utils

getAt :: Int -> [a] -> Maybe a
getAt _ [] = Nothing
getAt 0 (x : _) = Just x
getAt n (_ : xs) = getAt (n - 1) xs

setAt :: Int -> a -> [a] -> [a]
setAt i a ls
  | i < 0 = ls
  | otherwise = go i ls
  where
    go 0 (_ : xs) = a : xs
    go n (x : xs) = x : go (n -1) xs
    go _ [] = []
