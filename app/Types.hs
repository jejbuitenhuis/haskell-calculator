module Types (
    AST (..),
)
    where

data AST = SomeNumber (Maybe Double)
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         | Expr AST
    deriving (Show)
