{-# LANGUAGE GADTs #-}

module Bool where

import Prelude hiding (Bool (..), Num (..))

data Bool where
  False :: Bool
  True :: Bool
  deriving (Show)

band :: Bool -> Bool -> Bool
band True True = True
band _ _ = False

bor :: Bool -> Bool -> Bool
bor False False = False
bor _ _ = True
