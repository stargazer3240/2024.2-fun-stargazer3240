module Nat where

import Prelude hiding (Num (..))

data Nat = O | S Nat
  deriving (Eq, Show)

plus :: Nat -> Nat -> Nat
plus n O = n
plus n (S m) = S (plus n m)

times :: Nat -> Nat -> Nat
times n O = O
times n (S m) = plus (times n m) n

power :: Nat -> Nat -> Nat
power n O = S O
power n (S m) = times (power n m) n
