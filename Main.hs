module Main where

import System.IO
import System.Environment
import Data.List
import Solver

-- TODO: stretch: print out merchants

main :: IO()
main = do
    args <- getArgs
    case args of
        [file] -> do
            content <- readFile file
            let list = lines content
            let merchantVisits = [map read $ words s | s <- list] :: [[Int]]
            let maybeBoard = solve merchantVisits
            case maybeBoard of
                Just board -> printBoard board
                Nothing -> putStrLn "No valid Zed map for these merchant visits"
        _ -> putStrLn "Wrong number of arguments"


printBoardLine :: Show a => [a] -> IO ()
printBoardLine line = putStrLn (unwords (map show line))

printBoard :: (Foldable t) => t [Int] -> IO ()
printBoard board = mapM_ printBoardLine board
