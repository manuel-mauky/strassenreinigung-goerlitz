module Main where

  import Scrape

  main :: IO ()
  main = do
    dates <- extractValues "test/simplified.html"
    mapM_ putStrLn dates
