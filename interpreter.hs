import Prelude hiding (lookup)

type M a = a
unitM a = a
a `bindM` f = f a
showM = showVal

type O a = (String, a)
unitO a  = ("", a)
m `bindO` f = let (r, a) = m
                  (s, b) = f a
              in (r ++ s, b)
showO (s, a) = "Output: " ++ s ++ " Value: " ++ showVal a

outO :: Value -> O ()
outO a = (showVal a ++ "; ", ())

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term
    | Out Term

data Value = Wrong
    | Num Int
    | Fun (Value -> O Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> O Value
interp (Con i) e = unitO (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindO` (\x ->
                     interp v e `bindO` (\y ->
                     add x y))
interp (Lam n t) e = unitO (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindO` (\x ->
                     interp v e `bindO` (\y ->
                     apply x y))
interp (Out t) e = interp t e `bindO` (\a ->
                   outO a `bindO` (\() ->
                   unitO a))

lookup :: Name -> Environment -> O Value
lookup n [] = unitO Wrong
lookup n ((a,b):ax)
    | a == n = unitO b
    | otherwise = lookup n ax

add :: Value -> Value -> O Value
add (Num i) (Num j) = unitO (Num (i+j))
add u v = unitO Wrong

apply :: Value -> Value -> O Value
apply (Fun f) v = f v
apply u v = unitO Wrong

test :: Term -> String
test u = showO $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))
term4 = (Add (Out (Con 41)) (Out (Con 1)))
