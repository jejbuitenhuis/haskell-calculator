module Types (
    AST (..),
)
    where

data AST = Number (Maybe Double)
         | Add
         | Sub
         | Mul
         | Div
         | Expression [AST]
          deriving (Show, Eq)
