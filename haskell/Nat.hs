module Nat where

import Prelude hiding (Num (..), pred, (^))

data Nat = O | S Nat

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

-- double using only recursion
-- double O = O
-- double (S O) = S (S O)
-- double (S n) = S (S (double n))

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

instance Show Nat where
  show O = "0"
  show (S n) = "S" ++ show n

-- Default Show:
-- instance Show Nat where
--   show O = "0"
--   show (S O) = "S 0"
--   show (S n) = "S (" ++ show n ++ ")"

instance Eq Nat where
  (==) O O = True
  (==) (S n) (S m) = n == m
  (==) _ _ = False
