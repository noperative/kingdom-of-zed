module Main where

import System.IO
import Data.List

-- TODO: compile into CLI
-- TODO: read from file
-- TODO: print out grid
-- TODO: stretch: print out merchants

main :: IO()
main = do
          content <- readFile "input.txt"
          let list = lines content
          let merchantVisits = [map read $ words s | s <- list] :: [[Int]]
          -- let board = solve merchantVisits and print that instead
          printBoard merchantVisits


printBoardLine :: Show a => [a] -> IO ()
printBoardLine line = putStrLn (unwords (map show line))

printBoard :: (Foldable t) => t [Int] -> IO ()
printBoard board = mapM_ printBoardLine board