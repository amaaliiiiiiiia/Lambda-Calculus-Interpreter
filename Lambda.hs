module Lambda where

import Data.List (nub, find, (\\))
import Data.Maybe (fromJust)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = nub (vars e1 ++ vars e2)
vars (Abs x e) = nub (x : vars e)

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2)
freeVars (Abs x e) = nub (filter (/= x) (freeVars e)) -- elimin toate ap lui x din e

-- generez variabile posibile de diferite lungimi
generateVars :: [String]
generateVars = concatMap varsOfLength [1..]
  where
    -- lista cu toate literele din alfabet
    letters = ['a'..'z']
    -- replic alfabetul de n ori si creez secvente cu cate o litera din toate cele n liste
    varsOfLength n = sequence (replicate n letters)

-- 1.3.
newVar :: [String] -> String
-- cauta primul element din lista generateVars care nu a fost folosit
newVar list = fromJust (find (`notElem` list) generateVars)

isAbs :: Lambda -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = isNormalForm e
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2 && not (isAbs e1)

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = case e1 of
    Var v ->
        if v == x
            then e2
        else e1
    App e1' e2' -> App (reduce x e1' e2) (reduce x e2' e2)
    Abs e1' e2' ->
        if e1' == x
            then Abs e1' e2'
        else if e1' `elem` freeVars e2 then
            let z = newVar (vars e2' ++ vars e2)
                e2'' = reduce e1' e2' (Var z)
            in Abs z (reduce x e2'' e2)
        else Abs e1' (reduce x e2' e2)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2
normalStep (App e1 e2)
    | not (isNormalForm e1) = App (normalStep e1) e2
    | isAbs e1 = App (normalStep e1) e2
    | not (isNormalForm e2) = App e1 (normalStep e2)
    | otherwise = App e1 e2
normalStep (Abs x e) = Abs x (normalStep e)
normalStep x = x

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2)
    | isNormalForm e2 = reduce x e1 e2
    | otherwise = App (Abs x e1) (applicativeStep e2)
applicativeStep (App e1 e2)
    | not (isNormalForm e1) = App (applicativeStep e1) e2
    | not (isNormalForm e2) = App e1 (applicativeStep e2)
    | otherwise = App e1 e2
applicativeStep (Abs x e) = Abs x (applicativeStep e)
applicativeStep x = x

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify f e
    | isNormalForm e = [e]
    | otherwise = e : simplify f (f e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
