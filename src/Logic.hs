-- Module that contains and handles all the logic and operations.
module Logic (getTable) where

data ExpBool
  = Var Char
  | Not ExpBool
  | Or ExpBool ExpBool
  | And ExpBool ExpBool
  | Implies ExpBool ExpBool
  | Equal ExpBool ExpBool

instance Show ExpBool where
  show (Var p) = show p
  show (Not exp1) = "~(" ++ (show exp1) ++ ")"
  show (Or exp1 exp2) = "(" ++ (show exp1) ++ "v" ++ (show exp2) ++ ")"
  show (And exp1 exp2) = "(" ++ (show exp1) ++ "^" ++ (show exp2) ++ ")"
  show (Implies exp1 exp2) = "(" ++ (show exp1) ++ "=>" ++ (show exp2) ++ ")"
  show (Equal exp1 exp2) = "(" ++ (show exp1) ++ "<=>" ++ (show exp2) ++ ")"

getTable :: String -> String
getTable str = str