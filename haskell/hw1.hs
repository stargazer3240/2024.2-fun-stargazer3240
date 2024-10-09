import Nat

import Prelude hiding (pred)

double :: Nat -> Nat
double = times (S (S O))

fact :: Nat -> Nat
fact O = S O
fact (S n) = times (S n) (fact n)

fib :: Nat -> Nat
fib (S O) = S O
fib (S (S O)) = S O
fib (S (S n)) = plus (fib (S n)) (fib n)

pred :: Nat -> Nat
pred O = O
pred (S n) = n
