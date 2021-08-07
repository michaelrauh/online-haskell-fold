module Lib
    ( makeNextMapping
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (tails)
import Data.List.Split ( splitOn )


windows :: [String] -> [(String, String)]
windows x = zip x (drop 1 x)

makeNextMapping :: String -> Map.Map String (Set.Set String)
makeNextMapping str = let words = windows(splitOn " " str) in foldr addWordToCorr Map.empty words

addWordToCorr :: (String, String) -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
addWordToCorr (f, s) acc = Map.unionWith Set.union acc (Map.fromList [(f, Set.singleton s)])