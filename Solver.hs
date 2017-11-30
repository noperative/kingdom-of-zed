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

-- TODO:

-- takes a map [[a]] and the merchant visits [[a]] and return
--  a bool for whether the map is valid or not
isValid :: [[a]] -> [[a]] -> Bool
isValid map merchantVisits = (validBoard map) && (validMerchants map merchantVisits)


-- verifies that the map is all unique rows and columns
validBoard :: [[a]] -> Bool

-- verifies that the merchant visits are satisfied by the board
validMerchants :: [[a]] -> [[a]] -> Bool



