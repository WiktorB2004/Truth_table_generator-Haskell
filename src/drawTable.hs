-- Module for to draw a table
module DrawTable
  ( drawCells,
    drawLines,
    drawLines2,
  )
where

-- Draws cells of table.
drawCells :: [Char] -> String
drawCells [] = "|"
drawCells (x : xs) = "| " ++ [x] ++ " " ++ drawCells xs

-- Draws lines of tables.
drawLines :: [Char] -> String
drawLines [] = "\n"
drawLines (_ : xs) = "--" ++ drawLines xs

-- Draws lines of tables.
drawLines2 :: [Char] -> String
drawLines2 [] = "\n"
drawLines2 (_ : xs) = "----" ++ drawLines2 xs