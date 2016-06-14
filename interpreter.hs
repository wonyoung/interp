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

type Position = Int
showpos = show
pos0 = 0

type P a = Position -> E a
unitP a = \p -> unitE a
errorP s = \p -> errorE (showpos p ++ ": " ++ s)
m `bindP` f = \p -> m p `bindE` (\x -> f x p)
    -- m :: P a
    -- f :: a -> P a
    -- m p :: E a
    -- f x :: P a
    -- f x p :: E a
    -- (\x -> f x p) :: a -> E a
showP m = showE (m pos0)
resetP :: Position -> P x -> P x
resetP q m = \p -> m q

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term
    | At Position Term

data Value = Wrong
    | Num Int
    | Fun (Value -> P Value)

type Environment = [(Name, Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> P Value
interp (Con i) e = unitP (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindP` (\x ->
                     interp v e `bindP` (\y ->
                     add x y))
interp (Lam n t) e = unitP (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindP` (\x ->
                     interp v e `bindP` (\y ->
                     apply x y))
interp (At p t) e = resetP p (interp t e)

lookup :: Name -> Environment -> P Value
lookup n [] = errorP ("unbound variable: " ++ n)
lookup n ((a,b):ax)
    | a == n = unitP b
    | otherwise = lookup n ax

add :: Value -> Value -> P Value
add (Num i) (Num j) = unitP (Num (i+j))
add u v = errorP ("should be numbers: " ++ showVal u ++ ", " ++ showVal v)

apply :: Value -> Value -> P Value
apply (Fun f) v = f v
apply u v = errorP ("should be function: " ++ showVal u)

test :: Term -> String
test u = showP $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))
term1 =
  (At 1
    (App
      (Lam "x" (Add (Var "x") (Var "x")))
      (At 3
        (Add (Con 11) (Var "y")))))
