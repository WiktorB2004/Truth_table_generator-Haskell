-- Implementation of stack

module StackString
  ( StackString,
    isEmpty,
    push,
    pop,
  )
where

type StackString = [Char]

isEmpty :: StackString -> Bool
isEmpty [] = True
isEmpty _ = False

push :: StackString -> Char -> StackString
push stack c = c : stack

pop :: StackString -> StackString
pop (x : xs) = xs