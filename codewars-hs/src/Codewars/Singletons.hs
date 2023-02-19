-- https://www.codewars.com/kata/54750ed320c64c64e20002e2

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Codewars.Singletons where

import Prelude hiding (drop, head, map, replicate, tail, take, zipWith, (++))

data Vec a n where
  VNil :: Vec a 'Zero
  VCons :: a -> Vec a n -> Vec a ('Succ n)

data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat 'Zero
  SSucc :: SNat a -> SNat ('Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool

type instance m :< 'Zero = 'False
type instance 'Zero :< 'Succ n = 'True
type instance ('Succ m) :< ('Succ n) = m :< n

type family Add (a :: Nat) (b :: Nat) :: Nat

type instance Add 'Zero b = b
type instance Add ('Succ a) b = 'Succ (Add a b)

type family Sub (a :: Nat) (b :: Nat) :: Nat

type instance Sub 'Zero a = 'Zero
type instance Sub a 'Zero = a
type instance Sub ('Succ a) ('Succ b) = Sub a b

type family Min (a :: Nat) (b :: Nat) :: Nat

type instance Min a 'Zero = 'Zero
type instance Min 'Zero b = 'Zero
type instance Min ('Succ a) ('Succ b) = 'Succ (Min a b)

map :: (a -> b) -> Vec a n -> Vec b n
map _ VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ 'True) => SNat a -> Vec s b -> s
index SZero (VCons h _) = h
index (SSucc n) (VCons _ t) = index n t

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate x (SSucc n) = VCons x (replicate x n)

zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil VNil = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ ys = ys
(VCons x xs) ++ ys = VCons x (xs ++ ys)

take :: SNat n -> Vec v m -> Vec v (Min n m)
take _ VNil = VNil
take SZero _ = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

drop :: SNat n -> Vec v m -> Vec v (Sub m n)
drop SZero v = v
drop _ VNil = VNil
drop (SSucc n) (VCons _ xs) = drop n xs

head :: (('Zero :< n) ~ 'True) => Vec v n -> v
head (VCons x _) = x

tail :: (('Zero :< 'Succ n) ~ 'True) => Vec v ('Succ n) -> Vec v n
tail (VCons _ xs) = xs
