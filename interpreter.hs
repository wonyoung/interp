import Prelude hiding (lookup)

type M a = a
unitM a = a
a `bindM` f = f a
showM = showVal

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term

data Value = Wrong
    | Num Int
    | Fun (Value -> M Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> M Value
interp (Con i) e = unitM (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindM` (\x ->
                     interp v e `bindM` (\y ->
                     add x y))
interp (Lam n t) e = unitM (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindM` (\x ->
                     interp v e `bindM` (\y ->
                     apply x y))

lookup :: Name -> Environment -> M Value
lookup n [] = unitM Wrong
lookup n ((a,b):ax)
    | a == n = unitM b
    | otherwise = lookup n ax

add :: Value -> Value -> M Value
add (Num i) (Num j) = unitM (Num (i+j))
add u v = unitM Wrong

apply :: Value -> Value -> M Value
apply (Fun f) v = f v
apply u v = unitM Wrong

test :: Term -> String
test u = showM $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))

