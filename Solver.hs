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


{- isValid   [ [1,2,3,4]
                [2,3,4,1]
                [3,4,1,2]
                [4,1,2,3] ]
-}

-- TODO:
-- takes a board [[a]] and the merchant visits [[a]] and return
--  a bool for whether the map is valid or not
isValid :: Eq a => [[a]] -> [[a]] -> Bool
isValid board merchantVisits = (validBoard board) && (validMerchants board merchantVisits)


{- isValid   [ [1,2,3,4]
                [2,3,4,1]
                [3,4,1,2]
                [4,1,2,3] ]
-}

-- verifies that the map is all unique rows and columns
validBoard :: Eq a => [[a]] -> Bool
validBoard board = 	
          (foldr (\x y -> allUnique x && y) True rows ) && 
					(foldr (\x y -> allUnique x && y) True columns)
	where
		rows = board
		columns = transpose board

-- helper for verifying all elements in a list are unique
allUnique :: Eq t => [t] -> Bool
allUnique [] = True
allUnique (h:t)
	| elem h t 	= False
	| otherwise = allUnique t

-- verifies that the merchant visits are satisfied by the board
validMerchants :: [[a]] -> [[a]] -> Bool
validMerchants board visits = False


