module Parser (
    execute,
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

evaluateNum :: AST -> Maybe Double -> Maybe Double -> Maybe Double
evaluateNum Add (Just a) (Just b) = Just $ a + b
evaluateNum Sub (Just a) (Just b) = Just $ a - b
evaluateNum Mul (Just a) (Just b) = Just $ a * b
evaluateNum Div (Just a) (Just b) = Just $ a / b
evaluateNum _ _ _ = Nothing

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

evaluate :: Map AST -> AST -> Maybe Double
evaluate _ ( Expression [Number num] ) = num
evaluate _ (Expression ((Expression curr):_)) = evaluate expressions $ Expression curr
evaluate ((_, operatorFunc):rest) (Expression ast) =
    case span (/= operatorFunc) ast of
      (_, []) -> evaluate rest (Expression ast)
      (left, right) -> let before = Expression left
                           after = Expression $ drop 1 right
                        in evaluateNum operatorFunc
                             (evaluate expressions before)
                             (evaluate expressions after)
evaluate _ _ = Nothing

execute :: String -> Maybe Double
-- execute = evaluate expressions . parse
execute input = let ast = parse input
                 in evaluate expressions ast `debug` show ast
