-- TODO(#1): Make it possible to enter formulas in mathematical way. (=>, /\, \/, <=>)
-- TODO(#2): Edit the logic so formulas would look like A OPERATOR B instead of OPERATOR A B

-- Module that contains and handles all the logic and operations.

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
  show (And exp1 exp2) = "(" ++ show exp1 ++ "/\\" ++ show exp2 ++ ")"
  show (Implies exp1 exp2) = "(" ++ show exp1 ++ "=>" ++ show exp2 ++ ")"
  show (Equal exp1 exp2) = "(" ++ show exp1 ++ "<=>" ++ show exp2 ++ ")"

-- Analyzes syntax of the provided logic formula.
getExpBool :: String -> ExpBool
getExpBool [p] =
  if fromEnum p > 64 && fromEnum p < 123 && p /= 'v'
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
  [("~", _ : xs)] -> Not (getExpBool xs)
  [("\\/", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Or (getExpBool exp1) (getExpBool exp2)
  [("/\\", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in And (getExpBool exp1) (getExpBool exp2)
  [("=>", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Implies (getExpBool exp1) (getExpBool exp2)
  [("<=>", _ : rest)] ->
    let (exp1, exp2) = clearExpBin rest
     in Equal (getExpBool exp1) (getExpBool exp2)
  _ -> error "Sintaxis error"

-- Funtion auxiliar, which clears a logic formula.
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
getAtomicsVar :: ExpBool -> String
getAtomicsVar exp1 = noRepeat (getVars exp1)

getVars :: ExpBool -> String
getVars (Var p) = [p]
getVars (Not exp1) = getVars exp1
getVars (Or exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (And exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (Implies exp1 exp2) = getVars exp1 ++ getVars exp2
getVars (Equal exp1 exp2) = getVars exp1 ++ getVars exp2

noRepeat :: String -> String
noRepeat [] = []
noRepeat (x : xs) =
  if x `elem` xs
    then noRepeat xs
    else x : noRepeat xs

-- Form the power set of a list to obtain all interpretations.
subsuc :: [a] -> [[a]]
subsuc [] = [[]]
subsuc (c : xs) = subsuc xs ++ [c : ys | ys <- subsuc xs]

-- Evaluate an interpretation in a logic formula
evaluate :: ExpBool -> String -> Bool
evaluate (Var p) vars = p `elem` vars
evaluate (Not exp1) vars = not (evaluate exp1 vars)
evaluate (Or exp1 exp2) vars = evaluate exp1 vars || evaluate exp2 vars
evaluate (And exp1 exp2) vars = evaluate exp1 vars && evaluate exp2 vars
evaluate (Implies exp1 exp2) vars = not (evaluate exp1 vars) || evaluate exp2 vars
evaluate (Equal exp1 exp2) vars = evaluate exp1 vars == evaluate exp2 vars

-- Construct the truth table of a propositional logic formula.
getTable :: String -> String
getTable str =
  let exp1 = getExpBool str
   in auxTable exp1 (getAtomicsVar exp1)

auxTable :: ExpBool -> [Char] -> String
auxTable exp1 vars =
  let str = show exp1
   in drawLines (vars ++ str)
        ++ drawCells vars
        ++ " "
        ++ str
        ++ " |\n"
        ++ aux2Table exp1 vars (subsuc vars)

aux2Table :: ExpBool -> [Char] -> [[Char]] -> String
aux2Table _ _ [] = ""
aux2Table exp1 vars (x : xs) =
  let values = aux3Table vars x
   in if evaluate exp1 x
        then
          drawLines2 (values ++ "T")
            ++ drawCells (values ++ "T")
            ++ "\n"
            ++ aux2Table exp1 vars xs
        else
          drawLines2 (values ++ "F")
            ++ drawCells (values ++ "F")
            ++ "\n"
            ++ aux2Table exp1 vars xs

aux3Table :: [Char] -> [Char] -> [Char]
aux3Table [] _ = []
aux3Table (x : xs) inter =
  if x `elem` inter
    then 'T' : aux3Table xs inter
    else 'F' : aux3Table xs inter
