import Prelude hiding (lookup)

type M a = a
unitM a = a
a `bindM` f = f a
showM = showVal

type State = Int
showstate :: State -> String
showstate = show
s0 = 0

type S a = State -> (a, State)
unitS a = \s -> (a, s)
m `bindS` f = \s2 -> let (a, s0) = m s1
                         (b, s1) = f a s2
                     in  (b, s0)
showS m = let (a, s1) = m s0
          in  "Value: " ++ showVal a ++ "; " ++
              "Count: " ++ showstate s1

tickS :: S ()
tickS = \s -> ((), s+1)

fetchS :: S State
fetchS = \s -> (s, s)

type Name = String
data Term = Con Int
    | Var Name
    | Add Term Term
    | Lam Name Term
    | App Term Term
    | Count

data Value = Wrong
    | Num Int
    | Fun (S Value -> S Value)

type Environment = [(Name, S Value)]

showVal :: Value -> String
showVal Wrong = "<wrong>"
showVal (Fun f) = "<function>"
showVal (Num i) = show i

interp :: Term -> Environment -> S Value
interp (Con i) e = unitS (Num i)
interp (Var v) e = lookup v e
interp (Add u v) e = interp u e `bindS` (\x ->
                     interp v e `bindS` (\y ->
                     add x y))
interp (Lam n t) e = unitS (Fun (\x -> interp t ((n, x):e)))
interp (App u v) e = interp u e `bindS` (\x ->
                     apply x (interp v e))
interp Count e = fetchS `bindS` (\i -> unitS (Num i))

lookup :: Name -> Environment -> S Value
lookup n [] = unitS Wrong
lookup n ((a,b):ax)
    | a == n = b
    | otherwise = lookup n ax

add :: Value -> Value -> S Value
add (Num i) (Num j) = tickS `bindS` (\() -> unitS (Num (i+j)))
add u v = unitS Wrong

apply :: Value -> S Value -> S Value
apply (Fun f) v = tickS `bindS` (\() -> f v)
apply u v = unitS Wrong

test :: Term -> String
test u = showS $ interp u []

term0 = (App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 11) (Con 10)))
term2 = (Add (Add (Con 1) (Con 2)) Count)
