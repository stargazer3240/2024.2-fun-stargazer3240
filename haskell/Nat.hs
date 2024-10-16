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
n ^ (S m) = n ^ m * n

-- abbrevs
o, so, sso, ssso :: Nat
o = O
so = S o
sso = S so
ssso = S sso

double :: Nat -> Nat
double n = n * sso

fact :: Nat -> Nat
fact O = S O
fact (S n) = S n * fact n

fib :: Nat -> Nat
fib (S O) = S O
fib (S (S O)) = S O
fib (S (S n)) = fib (S n) + fib n

pred :: Nat -> Nat
pred (S n) = n
pred _ = error "0 doesn't have pred!"
