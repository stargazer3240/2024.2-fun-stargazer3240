module Nat where

import Prelude hiding (Num (..), (^))

data Nat = O | S Nat
  deriving (Eq, Show)

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S (n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = n * m + n

(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = (n ^ m) * n
