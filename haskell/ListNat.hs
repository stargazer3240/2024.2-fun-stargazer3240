{-# LANGUAGE GADTs #-}

module ListNat where

import Nat
import Prelude hiding (Num (..))

data ListNat where
  Empty :: ListNat
  Constructor :: Nat -> ListNat -> ListNat
