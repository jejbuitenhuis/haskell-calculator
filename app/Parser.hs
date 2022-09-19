module Parser (
    parse,
)
    where

import Util
import Types

parse :: String -> AST
parse input = parse' $ words input
    where
        parse' [] = error "Unexpected end of input"
        parse' [curr] = SomeNumber $ stringToDouble curr
        parse' [curr, ")"] = SomeNumber $ stringToDouble curr
        parse' [_, _] = error "Unexpected state of input"
        parse' (first:second:list)
          | first == "(" = Expr $ parse' $ second : list
          | second == "+" = Add (SomeNumber $ stringToDouble first) (parse' list)
          | second == "-" = Sub (SomeNumber $ stringToDouble first) (parse' list)
          | second == "*" = Mul (SomeNumber $ stringToDouble first) (parse' list)
          | second == "/" = Div (SomeNumber $ stringToDouble first) (parse' list)
          | otherwise = error "Unexpected character"
