module Types where

import Data.Time

data Street = Street { name :: String}
  deriving (Show, Eq)

data CleaningDate = CleaningDate { date :: Day }
  deriving (Show, Eq)
