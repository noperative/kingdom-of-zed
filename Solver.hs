module Solver where

import Data.List

solve merchantVisits =
	possibleMaps = generatePossibleMaps n
	findValidMap possibleMaps merchantVisits
    where
    	n = length head merchantVisits

findValidMap (map:maps) merchantVisits
	| isValid map merchantVisits = map
	| otherwise = findValidMap maps merchantVisits

-- TODO:
generatePossibleMaps n = []


{- isValid [[4,1,3,2],[2,3,4,1],[3,2,1,4],[1,4,2,3]] [[1,3,2,2],[3,2,1,2],[2,2,1,3],[2,2,3,1]]
                -}

-- TODO:
-- takes a board [[a]] and the merchant visits [[a]] and return
--  a bool for whether the map is valid or not
isValid :: (Num a, Ord a) => [[a]] -> [[a]] -> Bool
isValid board merchantVisits = (validBoard board) && (validMerchants board merchantVisits)


{- isValid   [ [1,2,3,4]
                [2,3,4,1]
                [3,4,1,2]
                [4,1,2,3] ]
                -}

-- verifies that the map is all unique rows and columns
validBoard :: Ord a => [[a]] -> Bool
validBoard board =
  (foldr (\x y -> allUnique x && y) True rows ) &&
  (foldr (\x y -> allUnique x && y) True columns)
  where
    rows = board
    columns = transpose board

-- helper for verifying all elements in a list are unique
allUnique :: Ord t => [t] -> Bool
allUnique [] = True
allUnique (h:t)
  | elem h t  = False
  | otherwise = allUnique t

-- verifies that the merchant visits are satisfied by the board
validMerchants :: (Num a, Ord a) => [[a]] -> [[a]] -> Bool
validMerchants board visits =
  and ( (zipWith (\x y -> numVisits y 0 == x) north n_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) west  w_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) south s_dir) ++
        (zipWith (\x y -> numVisits y 0 == x) east  e_dir)  )
  where
    w_dir = board
    n_dir = transpose board
    e_dir = map reverse w_dir
    s_dir = map reverse n_dir
    north = visits !! 0
    east = visits !! 1
    south = reverse (visits !! 2)
    west = reverse (visits !! 3)

-- helper that returns how many visits a merchant would make
numVisits :: (Num a, Ord a) => [a] -> a  -> a
numVisits [] _ = 0
numVisits (h:t) n
  | n < h  = 1 + numVisits t h
  | otherwise = numVisits t n
