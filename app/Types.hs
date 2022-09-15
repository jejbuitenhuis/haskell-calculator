module Types (
    Operator (..),
    AST (..),
    execute,
)
    where

data BTree n = Leaf n | Branch (BTree n) (BTree n)
    deriving (Eq, Show)

data AST n = Expr (AST n) Operator (AST n) | Number (Maybe n)
    deriving (Show)

data Operator = Add | Sub | Div | Mul
    deriving (Show, Enum)

execute :: (Fractional a) => Operator -> Maybe a -> a -> Maybe a
execute Add a b = a >>= (\n -> Just $ n + b)
execute Sub a b = a >>= (\n -> Just $ n - b)
execute Mul a b = a >>= (\n -> Just $ n * b)
-- TODO: Some way to check if b == 0
execute Div a b = a >>= (\n -> Just $ n / b)
