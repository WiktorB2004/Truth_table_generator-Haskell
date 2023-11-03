module IoHandle (inputOutput) where

import Logic

-- Manages input and output of program.
inputOutput :: IO ()
inputOutput = do
  putStrLn "Enter a propositional logic formula:"
  str <- getLine
  putStrLn ""
  putStr (getTable str)