-- https://www.codewars.com/kata/5cd99b8af446b0000ed8e615

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}

module Codewars.LensApprentice where

newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Show)

instance Functor Identity where
  fmap f = Identity . f . runIdentity

newtype Const a b = Const {getConst :: a} deriving (Eq, Show)

instance Functor (Const a) where
  fmap _ = Const . getConst

type Lens s t a b = forall f. Functor f => (a -> f b) -> (s -> f t)

type Lens' s a = Lens s s a a

view :: Lens s t a b -> s -> a
view l = getConst . l Const

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)

set :: Lens s t a b -> b -> s -> t
set l = over l . const

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x,) <$> f a

data Person = Person {name :: String, age :: Int} deriving (Eq, Show)

_name :: Lens' Person String
_name f p = (\name' -> p {name = name'}) <$> f (name p)

newtype TempC = TempC {getC :: Float} deriving (Eq, Show, Num)

newtype TempF = TempF {getF :: Float} deriving (Eq, Show, Num)

c_f :: TempC -> TempF
c_f (TempC c) = TempF $ (9 / 5 * c) + 32

f_c :: TempF -> TempC
f_c (TempF f) = TempC $ 5 / 9 * (f - 32)

_celsius :: Lens' TempF TempC
_celsius f t = c_f <$> (f . f_c $ t)

_1_1_1_name :: Lens' (((Person, x), y), z) String
_1_1_1_name = _1 . _1 . _1 . _name

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter f s = setter s <$> (f . getter) s
