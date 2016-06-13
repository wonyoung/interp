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

interp :: Term -> Environment -> Value
interp (Con i) e = Num i
interp (Var v) e = lookup v e
interp (Add u v) e = add (interp u e) (interp v e)
interp (Lam n t) e = Fun (\x -> interp t ((n, x):e))
interp (App u v) e = apply (interp u e) (interp v e)

lookup :: Name -> Environment -> Value
lookup n [] = Wrong
lookup n ((a,b):ax)
    | a == n = b
    | otherwise = lookup n ax

add :: Value -> Value -> Value
add (Num i) (Num j) = Num (i+j)
add u v = Wrong

apply :: Value -> Value -> Value
apply (Fun f) v = f v
apply u v = Wrong

test :: Term -> String
test u = showI $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))

