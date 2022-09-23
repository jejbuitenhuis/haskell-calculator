module Util (
    Map,
    insideMap,
    getFromMap,
    stringToDouble,
    stringIsNumber,
    debug,
)
    where

import Text.Read (readMaybe)
import Debug.Trace (trace)
import Data.Char (isDigit)

-- Custom map {{{
type Map a = [(String, a)]

insideMap :: String -> Map a -> Bool
insideMap _ [] = False
insideMap needle (current:haystack)
  | fst current == needle = True
  | otherwise = insideMap needle haystack

getFromMap :: String -> Map a -> a
getFromMap _ [] = error "Item not found in map"
getFromMap needle (current:haystack)
  | fst current == needle = snd current
  | otherwise = getFromMap needle haystack
-- }}}

stringToDouble :: String -> Maybe Double
stringToDouble s = readMaybe s :: Maybe Double

stringIsNumber :: String -> Bool
stringIsNumber = all (\n -> isDigit n || n == '-' || n == '.')

debug :: c -> String -> c
debug = flip trace
