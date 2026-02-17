module Parser (parseLambda, parseLine) where

import Control.Monad
import Control.Applicative

import Lambda
import Binding
import Data.Char (isAlpha, isSpace, isAlphaNum)

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

-- elimina spatiile de la inceputul unui sir
trim :: String -> String
trim = dropWhile isSpace

-- 2.1. / 3.2.
parseLambda :: String -> Lambda
parseLambda x = fst (parseExpresion (trim x)) -- apelez parseExpresion dupa ce elimin spatiile

parseExpresion :: String -> (Lambda, String)
parseExpresion e = case trim e of
    -- aplicatie
    ('(' : rest) ->
        let (e1, rest1) = parseExpresion rest
            (e2, rest2) = parseExpresion rest1
        in (App e1 e2, tail rest2)
    -- abstractie
    ('\\' : rest) ->
        let (v, '.':rest1) = span isAlpha rest -- dupa var urmeaza "."
            (e, rest2) = parseExpresion rest1
        in (Abs v e, rest2)
    -- variabila sau macro
    e' ->
        let (v, rest) = span isAlphaNum e'
        in if all (\c -> c `elem` ['A'..'Z'] ++ ['0'..'9']) v -- daca s-au citit doar litere mari si cifre, atunci e macro
            then (Macro v, rest)
            else (Var v, rest) -- altfel, e variabila

-- imparte string-ul la semnul "="
splitAtEqual :: String -> (String, String)
splitAtEqual [] = ([], [])
splitAtEqual (c : cs)
    | c == '=' = ([], cs)
    | otherwise =
        let (l, r) = splitAtEqual cs
        in (c : l, r)

-- 3.3.
parseLine :: String -> Either String Line
parseLine line = undefined
