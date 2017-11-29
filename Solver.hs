module Solver where

import Data.List

solve merchantVisits =
	possibleMaps = generatePossibleMaps n
	findValidMap possibleMaps    
    where
    	n = length head merchantVisits

findValidMap (map:maps)
	| isValid map = map
	| otherwise = findValidMap maps

-- TODO:
generatePossibleMaps n = []

-- TODO:
isValid map = False
