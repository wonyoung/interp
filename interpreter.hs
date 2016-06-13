import Prelude hiding (lookup)

type I a = a
unitI a = a
a `bindI` f = f a
showI = showVal

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term

data Value = Wrong
    | Num Int
    | Fun (Value -> I Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> I Value
interp (Con i) e = unitI (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindI` (\x ->
                     interp v e `bindI` (\y ->
                     add x y))
interp (Lam n t) e = unitI (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindI` (\x ->
                     interp v e `bindI` (\y ->
                     apply x y))

lookup :: Name -> Environment -> I Value
lookup n [] = unitI Wrong
lookup n ((a,b):ax)
    | a == n = unitI b
    | otherwise = lookup n ax

add :: Value -> Value -> I Value
add (Num i) (Num j) = unitI (Num (i+j))
add u v = unitI Wrong

apply :: Value -> Value -> I Value
apply (Fun f) v = f v
apply u v = unitI Wrong

test :: Term -> String
test u = showI $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))

