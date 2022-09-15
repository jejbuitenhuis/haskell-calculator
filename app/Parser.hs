module Parser (
    parse,
)
    where

import Util
import Types

parse :: String -> AST Double
parse input = parse' $ words input

parse' :: [String] -> AST Double
parse' [] = Number Nothing
parse' [num] = Number $ stringToDouble num
parse' [num, ")"] = Number $ stringToDouble num
parse' [_,_] = Number Nothing
parse' (first:second:third:input)
  | first == "(" = parse' $ arrUntil (== ")") $ second : third : input
  | second == "+" = Expr (parse' [first]) Add (parse' $ third : input)
  | second == "-" = Expr (parse' [first]) Sub (parse' $ third : input)
  | second == "*" = Expr (parse' [first]) Mul (parse' $ third : input)
  | second == "/" = Expr (parse' [first]) Div (parse' $ third : input)
  | otherwise = Number Nothing
