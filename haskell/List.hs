{-# LANGUAGE GADTs #-}

module List where

import Nat
import Prelude hiding (length, (+))

data List a where
  Nill :: List a
  Cons :: a -> List a -> List a
  deriving (Eq, Show)

length :: List a -> Nat
length Nill = O
length (_ `Cons` xs) = S (length xs)

fold :: a -> (a -> a -> a) -> List a -> a
fold r b Nill = r
fold r b (x `Cons` xs) = x `b` ((fold r b) xs)
