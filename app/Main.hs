module Main where

  import Scrape

  import Types

  showTmp :: (Street, CleaningDate) -> String
  showTmp (street, cleaningDate) = (name street) ++ " -> " ++ show (date cleaningDate)

  main :: IO ()
  main = do
    values <- loadData "test/simplified.html"

    mapM_ (putStrLn . showTmp) values
