module Scrape where

import Text.XML.HXT.Curl
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.Text as T


loadDoc :: String -> IOSArrow XmlTree XmlTree
loadDoc fileName = readDocument [withParseHTML yes
  , withWarnings no
  , withCurl []
  ] fileName


pretty :: IOSArrow XmlTree XmlTree -> IO [String]
pretty doc = runX . xshow $ doc >>> indentDoc

prettyPrint :: IOSArrow XmlTree XmlTree -> IO ()
prettyPrint doc = do
  res <- pretty doc
  mapM_ putStrLn res



selectDates :: IOSArrow XmlTree String
selectDates = css "#contentbereich p"
    >>> (ifA (hasAttrValue "class" (== "Quellenangabe")) (none) (this))  -- filter <p class="Quellenangabe">...
    /> getText


printDates :: IOSArrow XmlTree String -> IO ()
printDates dates = do
  result <- runX dates
  mapM_ putStrLn result


trim :: String -> String
trim = T.unpack . T.strip . T.pack

extractValues :: String -> IO [String]
extractValues uri = do
    let doc = loadDoc uri
    let filtered = doc >>> selectDates
    result <- runX filtered

    return $ filter (/= []) $ map trim result
