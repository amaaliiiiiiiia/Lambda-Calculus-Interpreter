module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step e = do
    let go (Var x) = Right (Var x)
        go (App e1 e2) = do
            e1' <- go e1
            e2' <- go e2
            Right (App e1' e2')
        go (Abs x e) = do
            e' <- go e
            Right (Abs x e')
        go (Macro m) =
            case lookup m ctx of -- cauta m in ctx
                Just val -> go val -- daca s-a gasit m
                Nothing -> Left m
    expanded <- go e -- se apeleaza functia go e care intoarce Either String Lambda
    return (simplify step expanded) -- aplic strategia normala sau aplicativa  pas cu pas
--    go inlocuieste macro-urile din expresie

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
