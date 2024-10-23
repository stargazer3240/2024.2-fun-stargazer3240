{-# LANGUAGE GADTs #-}

module List where

import Bool
import Nat
import Prelude hiding (Bool (..), filter, length, map, otherwise, prod, sum, (*), (+))

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

sum = fold O (+)
prod = fold (S O) (*)
conj = fold True band
disj = fold False bor

map :: (a -> b) -> (List a -> List b)
map f Nill = Nill
map f (x `Cons` xs) = f x `Cons` map f xs

filter :: (a -> Bool) -> (List a -> List a)
filter p Nill = Nill
filter p (x `Cons` xs) = ifthenelse (p x) (x `Cons` filter p xs) (filter p xs)
