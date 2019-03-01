module Scrape where

import Text.XML.HXT.Curl
import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.Text as T

import Data.Time
import Data.Maybe

import Text.ParserCombinators.ReadP
import Text.Parser.LookAhead

import Types

-- The main function of this module
-- It loads the street-cleaning data from the given uri.
-- The uri can be either a local filepath or a URI to a web resource.
loadData :: String -> IO [(Street, CleaningDate)]
loadData uri = do
  strings <- scrapeValues uri
  let values = parseValues strings
  return values

-- load the document from the given uri with HXT
loadDoc :: String -> IOSArrow XmlTree XmlTree
loadDoc fileName = readDocument [withParseHTML yes
  , withWarnings no
  , withCurl []
  ] fileName

-- HXT selector to get the street-cleaning values from the web document
selectDates :: IOSArrow XmlTree String
selectDates = css "#contentbereich p"
    >>> (ifA (hasAttrValue "class" (== "Quellenangabe")) (none) (this))  -- filter <p class="Quellenangabe">...
    /> getText

-- A helper function to remote leading and trailing spaces from a string
trim :: String -> String
trim = T.unpack . T.strip . T.pack

-- this function loads the document from the given uri and returns a list of strings,
-- each string representing a single line of data from the website
scrapeValues :: String -> IO [String]
scrapeValues uri = do
    let doc = loadDoc uri
    let filtered = doc >>> selectDates
    result <- runX filtered

    return $ filter (/= []) $ map trim result

-- A parser that finds dates from a String with the format "dd.mm.yyyy"
dayParser :: ReadP Day
dayParser = do
    day <- numbers 2
    string "."
    month <- numbers 2
    string "."
    year <- numbers 4

    case (fromGregorianValid year month day) of
      Nothing -> pfail
      Just day -> return day

    where
      numbers digits = fmap read (count digits digit)
      digit = satisfy (\char -> char >= '0' && char <= '9')

-- parses a line from the website and returns the street-name and date.
-- lines have the format "<streetname> dd.mm.yyyy"
parser :: ReadP (String, Day)
parser = do
  street <- manyTill anything (lookAhead dayParser)
  day <- dayParser

  return (street, day)
  where
    anything = satisfy (\char -> True)
    isDigit = \char -> char >= '0' && char <= '9'
    digit = satisfy isDigit

-- this function invokes the parser for a given line from the website
-- and returns a tuple with the actual data
parseValue :: String -> (Street, CleaningDate)
parseValue s = (Street (trim $ fst parseResult), CleaningDate (snd parseResult))
  where
    parseResult = fst $ head $ readP_to_S parser s

parseValues :: [String] -> [(Street, CleaningDate)]
parseValues = map parseValue
