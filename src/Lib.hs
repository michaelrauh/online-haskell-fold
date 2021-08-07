module Lib
    ( makeNextMapping
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (tails)
import Data.List.Split ( splitOn )
import Data.Char ( toLower )


windows :: [String] -> [(String, String)]
windows x = zip x (drop 1 x)

stripPunctuation :: String -> String
stripPunctuation = filter (`notElem` ",.?!-:;\"\'")

lower :: String -> String
lower = map toLower

makeNextMapping :: String -> Map.Map String (Set.Set String)
makeNextMapping str = let words = clean str in foldr addWordToCorr Map.empty words

clean :: String -> [(String, String)]
clean input = concatMap ((windows . words) . stripPunctuation) (splitOn ". " (lower input))

addWordToCorr :: (String, String) -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
addWordToCorr (f, s) acc = Map.unionWith Set.union acc (Map.fromList [(f, Set.singleton s)])