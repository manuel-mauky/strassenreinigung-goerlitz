module Types where

import           Data.Time

newtype Street = Street
  { name :: String
  } deriving (Show, Eq)

newtype CleaningDate = CleaningDate
  { date :: Day
  } deriving (Show, Eq)
