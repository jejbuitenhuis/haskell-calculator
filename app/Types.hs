module Types (
    AST1 (..),
    AST (..),
)
    where

data AST1 = SomeNumber (Maybe Double)
          | Add1 AST1 AST1
          | Sub1 AST1 AST1
          | Mul1 AST1 AST1
          | Div1 AST1 AST1
          | Expr1 AST1
    deriving (Show)

data AST = Number (Maybe Double)
         | Add
         | Sub
         | Mul
         | Div
         | Expression [AST]
          deriving (Show)
