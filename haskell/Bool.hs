{-# LANGUAGE GADTs #-}

module Bool where

import Prelude hiding (Bool (..), Num (..))

data Bool where
  False :: Bool
  True :: Bool
  deriving (Show)
