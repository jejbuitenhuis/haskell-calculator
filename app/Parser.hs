module Parser (
    parse,
)
    where

import Util
import Types

expressions :: Map AST
expressions = [
        ("+", Add),
        ("-", Sub),
        ("*", Mul),
        ("/", Div)
    ]

parse :: String -> AST
parse input = Expression $ snd $ parse' [] $ words input
    where
        parse' :: [AST] -> [String] -> (Int, [AST]) -- (length removed, AST)
        parse' acc [] = (0, acc)
        parse' acc (curr:rest)
          | curr == "(" =
              let parsed = parse' [] rest
                  outputLength = fst parsed
                  output = snd parsed
                  remaining = drop outputLength rest
                  rest' = parse' [] remaining
               in ( outputLength + fst rest' + 1, acc ++ [ Expression output ] ++ snd rest' )
          | curr == ")" = (1, acc)
          | curr `insideMap` expressions =
              let operator = curr `getFromMap` expressions
                  rest' = parse' (acc ++ [operator]) rest
               in ( fst rest' + 1, snd rest' )
          | stringIsNumber curr =
              let num = Number $ stringToDouble curr
                  rest' = parse' (acc ++ [num]) rest
               in ( fst rest' + 1, snd rest' )
          | otherwise = (0, acc)
