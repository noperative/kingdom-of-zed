module Solver where

import Data.List

-- solve takes in a list of lists of Ints representing the list of merchant visits,
-- with each list depicting one edge of the map, starting from the North side and progressing 
-- clockwise. Returns a valid Kingdom of Zed map as a matrix.
solve :: [[Int]] -> Maybe [[Int]]
solve merchantVisits =
    findValidMap possibleMaps merchantVisits
  where
    n = length (head merchantVisits)
    possibleMaps = generatePossibleMaps n


-- findValidMap takes in a list of Zed maps and finds the first one that is valid
-- according to the Zed rules. Will return a Maybe of a valid map.
findValidMap :: (Num a, Ord a) => [[[a]]] -> [[a]] -> Maybe [[a]]
findValidMap [] _ = Nothing
findValidMap (map:maps) visits
    | isValidMap map visits = Just map
    | otherwise = findValidMap maps visits


-- generatePossibleMaps takes in a Int representing the lengnth of a side of the Zed map.
-- It returns a list of all possible Zed maps with that side length.
generatePossibleMaps :: Int -> [[[Int]]]
generatePossibleMaps n = concatMap permutations (choose (permutations [1..n]) [])
  where
    choose [] result = if n == length result then [result] else []
    choose (x:xs) result
        | n == length result = [result]
        | otherwise = choose xs (x:result) ++ choose xs result


-- isValidMap takes a board in the form of a matrix and the merchant visits and returns
-- whether the map is valid or not according to rules of Zed.
isValidMap :: (Num a, Ord a) => [[a]] -> [[a]] -> Bool
isValidMap board merchantVisits =
    and ( (zipWith (\x y -> numVisits y 0 == x) north n_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) west  w_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) south s_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) east  e_dir)  )
  where
    w_dir = board
    n_dir = transpose board
    e_dir = map reverse w_dir
    s_dir = map reverse n_dir
    north = merchantVisits !! 0
    east = merchantVisits !! 1
    south = reverse (merchantVisits !! 2)
    west = reverse (merchantVisits !! 3)


-- numVisits returns how many visits a merchant would make
numVisits :: (Num a, Ord a) => [a] -> a  -> a
numVisits [] _ = 0
numVisits (h:t) n
    | n < h  = 1 + numVisits t h
    | otherwise = numVisits t n
