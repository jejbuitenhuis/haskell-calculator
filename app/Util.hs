module Util (
    arrUntil,
    arrUntilIncluding,
    breakIncluding,
    Map,
    insideMap,
    getFromMap,
    stringToDouble,
    debug,
)
    where

import Text.Read (readMaybe)
import Debug.Trace (trace)

-- Arrays {{{
arrUntil :: (a -> Bool) -> [a] -> [a]
arrUntil f = takeWhile (not . f)

arrUntilIncluding :: (a -> Bool) -> [a] -> [a]
arrUntilIncluding = arrUntilIncluding' []
    where
        arrUntilIncluding' acc _ [] = acc
        arrUntilIncluding' acc f (first:rest) = if f first
                                                   then arrUntilIncluding' (acc ++ [first]) f rest
                                                   else acc ++ [first] ++ [head rest]

breakIncluding :: (a -> Bool) -> [a] -> ([a], [a])
breakIncluding = breakIncluding' []
    where
        breakIncluding' acc _ [] = (acc,[])
        breakIncluding' acc f (first:rest) = if not . f $ first
                                                then breakIncluding' (acc ++ [first]) f rest
                                                else (acc ++ [first], rest)
-- }}}

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

debug :: c -> String -> c
debug = flip trace
