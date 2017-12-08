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
findValidMap (zedMap:zedMaps) visits
    | isValidMap zedMap visits = Just zedMap
    | otherwise = findValidMap zedMaps visits


-- generatePossibleMaps takes in a Int representing the lengnth of a side of the Zed map.
-- It returns a list of all possible Zed maps with that side length.
generatePossibleMaps :: Int -> [[[Int]]]
generatePossibleMaps n = concatMap permutations (choose (permutations [1..n]) [])
  where
    choose [] result = if n == length result then [result] else []
    choose (x:xs) result
        | n == length result = [result]
        | otherwise = choose xs (x:result) ++ choose xs result


-- isValidMap takes a map in the form of a matrix and the merchant visits and returns
-- whether the map is valid or not according to rules of Zed.
isValidMap :: (Num a, Ord a) => [[a]] -> [[a]] -> Bool
isValidMap zedMap merchantVisits = (uniqueRowsAndCols zedMap) && (validMerchants zedMap merchantVisits)


-- takes in a map and verifies that all rows and columns are unique
uniqueRowsAndCols :: Ord a => [[a]] -> Bool
uniqueRowsAndCols zedMap =
    (foldr (\x y -> allUnique x && y) True rows ) &&
    (foldr (\x y -> allUnique x && y) True columns)
  where
    rows = zedMap
    columns = transpose zedMap


-- checks whether all elements in a list are unique
allUnique :: Ord t => [t] -> Bool
allUnique [] = True
allUnique (h:t)
    | elem h t  = False
    | otherwise = allUnique t


-- takes in a map and a list of merchant visits and checks that
-- the number of trading posts each merchant visits is consistent
-- with the map
validMerchants :: (Num a, Ord a) => [[a]] -> [[a]] -> Bool
validMerchants zedMap visits =
    and ( (zipWith (\x y -> numVisits y 0 == x) north n_dir) ++
          (zipWith (\x y -> numVisits y 0 == x) west  w_dir) ++
          (zipWith (\x y -> numVisits y 0 == x) south s_dir) ++
          (zipWith (\x y -> numVisits y 0 == x) east  e_dir)  )
  where
    w_dir = zedMap
    n_dir = transpose zedMap
    e_dir = map reverse w_dir
    s_dir = map reverse n_dir
    north = visits !! 0
    east = visits !! 1
    south = reverse (visits !! 2)
    west = reverse (visits !! 3)


-- numVisits returns how many visits a merchant would make
numVisits :: (Num a, Ord a) => [a] -> a  -> a
numVisits [] _ = 0
numVisits (h:t) n
    | n < h  = 1 + numVisits t h
    | otherwise = numVisits t n
