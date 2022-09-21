module Parser (
    parse,
)
    where

import Util
import Types

parse1 :: String -> AST1
parse1 input = parse' $ words input
    where
        parse' [] = error "Unexpected end of input"
        parse' [curr] = SomeNumber $ stringToDouble curr
        parse' [curr, ")"] = SomeNumber $ stringToDouble curr
        parse' [_, _] = error "Unexpected state of input"
        parse' (first:second:list)
          | first == "(" = Expr1 $ parse' $ second : list
          | second == "+" = Add1 (SomeNumber $ stringToDouble first) (parse' list)
          | second == "-" = Sub1 (SomeNumber $ stringToDouble first) (parse' list)
          | second == "*" = Mul1 (SomeNumber $ stringToDouble first) (parse' list)
          | second == "/" = Div1 (SomeNumber $ stringToDouble first) (parse' list)
          | otherwise = error "Unexpected character"

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
        parse' acc [")"] = (1, acc)
        parse' acc [curr] = ( 1, acc ++ [ Number $ stringToDouble curr ] )
        parse' acc [curr, ")"] = ( 2, acc ++ [ Number $ stringToDouble curr ] )
        parse' acc [first, second]
          | first `insideMap` expressions =
              let operator = first `getFromMap` expressions
                  parsed = parse' [] [second]
               in ( 2, acc ++ [operator] ++ snd parsed )
          | otherwise = error ("Unexpected state of input. First:" ++ show first ++ " Second:" ++ show second)
        parse' acc (first:second:rest)
          | first == "(" =
              let parsed = parse' [] $ second : rest
                  outputLength = fst parsed
                  output = snd parsed
                  remaining = drop outputLength $ second : rest
                  rest' = parse' [] remaining
               in ( outputLength + fst rest', acc ++ [ Expression output ] ++ snd rest' )
          -- FIXME: When `1 + ( 2 + ( 3 + 4 ) )` is supplied, the output ends with a `Number Nothing`, which shouldn't be there
          | second == ")" =
              let parsedNum = parse' [] [first]
               in ( 1, acc ++ snd parsedNum )
          | second `insideMap` expressions =
              let acc' = parse' acc [first]
                  operator = second `getFromMap` expressions
                  rest' = parse' (snd acc' ++ [operator]) rest
               in ( fst acc' + fst rest' + 1, snd rest' )
          | otherwise = (0, acc)
