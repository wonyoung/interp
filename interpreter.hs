import Prelude hiding (lookup)

type M a = a
unitM a = a
a `bindM` f = f a
showM = showVal

type L a = [a]
unitL a = [a]
m `bindL` f = concatMap f m
zeroL = []
l `plusL` m = l ++ m
showL m = showlist . map showVal $ m
showlist :: [String] -> String
showlist xs = "[" ++ (_showlist xs) ++ "]"
_showlist (x:[]) = x
_showlist (x:xs) = x ++ "," ++ _showlist xs

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term
    | Fail
    | Amb Term Term

data Value = Wrong
    | Num Int
    | Fun (Value -> L Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> L Value
interp (Con i) e = unitL (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindL` (\x ->
                     interp v e `bindL` (\y ->
                     add x y))
interp (Lam n t) e = unitL (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindL` (\x ->
                     interp v e `bindL` (\y ->
                     apply x y))
interp Fail e = zeroL
interp (Amb u v) e = interp u e `plusL` interp v e

lookup :: Name -> Environment -> L Value
lookup n [] = unitL Wrong
lookup n ((a,b):ax)
    | a == n = unitL b
    | otherwise = lookup n ax

add :: Value -> Value -> L Value
add (Num i) (Num j) = unitL (Num (i+j))
add u v = unitL Wrong

apply :: Value -> Value -> L Value
apply (Fun f) v = f v
apply u v = unitL Wrong

test :: Term -> String
test u = showL $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))
term5 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Amb (Con 2) (Con 3))))
