import Prelude hiding (lookup)

type I a = a
unitI a = a
a `bindI` f = f a
showI = showVal

data E a = Success a | Error String
unitE a  = Success a
errorE s = Error s
(Success a) `bindE` f = f a
(Error s) `bindE` f = Error s
showE (Success a) = "Success: " ++ showVal a
showE (Error s)   = "Error: " ++ s

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term

data Value = Wrong
    | Num Int
    | Fun (Value -> E Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> E Value
interp (Con i) e = unitE (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindE` (\x ->
                     interp v e `bindE` (\y ->
                     add x y))
interp (Lam n t) e = unitE (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindE` (\x ->
                     interp v e `bindE` (\y ->
                     apply x y))

lookup :: Name -> Environment -> E Value
lookup n [] = unitE Wrong
lookup n ((a,b):ax)
    | a == n = unitE b
    | otherwise = lookup n ax

add :: Value -> Value -> E Value
add (Num i) (Num j) = unitE (Num (i+j))
add u v = unitE Wrong

apply :: Value -> Value -> E Value
apply (Fun f) v = f v
apply u v = unitE Wrong

test :: Term -> String
test u = showE $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))

