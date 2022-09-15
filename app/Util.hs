module Util (
    arrUntil,
    stringToDouble,
)
    where

import Text.Read (readMaybe)

arrUntil :: (a -> Bool) -> [a] -> [a]
arrUntil f = takeWhile (not . f)

stringToDouble :: String -> Maybe Double
stringToDouble s = readMaybe s :: Maybe Double
