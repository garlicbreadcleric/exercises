-- https://www.codewars.com/kata/54258ffb430ca2e4b5000239

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Codewars.Lensmaker where

import Data.Bitraversable (Bitraversable (bitraverse))
import Data.Monoid
import qualified Data.Traversable as T
import Data.Tuple (swap)
import Prelude hiding (sum)

class Profunctor p where
  dimap :: (a' -> a) -> (b -> b') -> (p a b -> p a' b')
  dimap f g = lmap f . rmap g
  lmap :: (a' -> a) -> (p a b -> p a' b)
  lmap f = dimap f id
  rmap :: (b -> b') -> (p a b -> p a b')
  rmap f = dimap id f

class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)

instance Profunctor (->) where
  dimap f g h = g . h . f

instance Choice (->) where
  left' f = either (Left . f) Right
  right' f = either Left (Right . f)

class Contravariant f where
  contramap :: (a' -> a) -> (f a -> f a')

newtype K b a = K {getK :: b} deriving (Functor)

instance Monoid b => Applicative (K b) where
  pure _ = K mempty
  K e <*> K f = K (e <> f)

instance Contravariant (K b) where
  contramap _ (K b) = K b

newtype Id a = Id {getId :: a} deriving (Functor)

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)

type Optic p f s t a b =
  p a (f b) -> p s (f t)

type Iso s t a b =
  forall p f.
  (Profunctor p, Functor f) =>
  Optic p f s t a b

type Lens s t a b =
  forall f.
  Functor f =>
  Optic (->) f s t a b

type Traversal s t a b =
  forall f.
  Applicative f =>
  Optic (->) f s t a b

type Fold s a =
  forall f.
  (Contravariant f, Applicative f) =>
  Optic (->) f s s a a

type Prism s t a b =
  forall p f.
  (Choice p, Applicative f) =>
  Optic p f s t a b

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (,x) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (x,) <$> f a

view :: Optic (->) (K a) s t a b -> (s -> a)
view l = getK . l K

over :: Optic (->) Id s t a b -> (a -> b) -> (s -> t)
over l f = getId . l (Id . f)

set :: Optic (->) Id s t a b -> b -> (s -> t)
set l = over l . const

elements :: T.Traversable g => Traversal (g a) (g b) a b
elements = traverse

toListOf :: Optic (->) (K (Endo [a])) s s a a -> (s -> [a])
toListOf l = flip appEndo [] . getK . l (\a -> K (Endo ([a] <>)))

preview :: Optic (->) (K (First a)) s s a a -> (s -> Maybe a)
preview l = getFirst . getK . l (K . First . Just)

coerce :: (Contravariant f, Functor f) => f a -> f b
coerce = contramap (const ()) . fmap (const ())

to :: (a -> b) -> Fold a b
to f g a = contramap f $ g (f a)

_Left :: Prism (Either a x) (Either b x) a b
_Left = rmap (bitraverse id pure) . left'

_Right :: Prism (Either x a) (Either x b) a b
_Right = rmap (bitraverse pure id) . right'

_flip :: Iso (a, b) (a, b) (b, a) (b, a)
_flip = dimap swap (swap <$>)
