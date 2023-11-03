{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Module that contains and handles all the logic and operations.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Logic (getTable) where

import DrawTable
import StackString

data ExpBool
  = Var Char
  | Not ExpBool
  | Or ExpBool ExpBool
  | And ExpBool ExpBool
  | Implies ExpBool ExpBool
  | Equal ExpBool ExpBool

instance Show ExpBool where
  show (Var p) = show p
  show (Not exp1) = "~(" ++ show exp1 ++ ")"
  show (Or exp1 exp2) = "(" ++ show exp1 ++ "v" ++ show exp2 ++ ")"
  show (And exp1 exp2) = "(" ++ show exp1 ++ "^" ++ show exp2 ++ ")"
  show (Implies exp1 exp2) = "(" ++ show exp1 ++ "=>" ++ show exp2 ++ ")"
  show (Equal exp1 exp2) = "(" ++ show exp1 ++ "<=>" ++ show exp2 ++ ")"

-- Analyzes syntax of the provided logic formula.
getExpBool :: String -> ExpBool
getExpBool [p] =
  if (fromEnum p > 64) && (fromEnum p < 123) && (p /= 'v')
    then Var p
    else error "Syntax error"
getExpBool ('(' : xs) =
  if last xs == ')'
    then getExpBool (init xs)
    else error "Syntax error"
getExpBool form = case lex form of
  [("Not", _ : xs)] -> Not (getExpBool xs)
  [("Or", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Or (getExpBool exp1) (getExpBool exp2)
  [("And", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in And (getExpBool exp1) (getExpBool exp2)
  [("Implies", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Implies (getExpBool exp1) (getExpBool exp2)
  [("Equal", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Equal (getExpBool exp1) (getExpBool exp2)
  _ -> error "Sintaxis error"

-- Funtion auxiliar, which clears a propositional logic formula.
clearExpBin :: String -> (String, String)
clearExpBin (x : xs) =
  if x == '('
    then split (x : xs) [] []
    else
      let (exp1, _ : rest) = head (lex (x : xs))
       in (exp1, rest)

-- Analyzes balanced parentheses.
split :: String -> String -> StackString -> (String, String)
split [] _ _ = error "Sintaxis error"
split (x : xs) exp1 stack
  | x == '(' = split xs (exp1 ++ [x]) (push stack x)
  | x == ')' = split xs (exp1 ++ [x]) (pop stack)
  | otherwise =
      if isEmpty stack
        then (exp1, xs)
        else split xs (exp1 ++ [x]) stack

-- gets Atomics formulae.
getAtomicsVar :: ExpBool -> [Char]
getAtomicsVar exp1 = noRepeat (getVars exp1)

getVars :: ExpBool -> [Char]
getVars (Var p) = [p]
getVars (Not exp1) = getVars exp1
getVars (Or exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (And exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (Implies exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (Equal exp1 exp2) = getVars exp1 ++ getVars exp2

noRepeat :: [Char] -> [Char]
noRepeat [] = []
noRepeat (x : xs) =
  if x `elem` xs
    then noRepeat xs
    else x : noRepeat xs

getTable :: String -> String
getTable str = str