module Lib
    ( makeNextMapping, makePrevMapping, makePhrases
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (tails, inits)
import Data.List.Split ( splitOn )
import Data.Char ( toLower )
import Data.Tuple ( swap )

windows :: [String] -> [(String, String)]
windows x = zip x (drop 1 x)

stripPunctuation :: String -> String
stripPunctuation = filter (`notElem` ",.?!-:;\"\'")

lower :: String -> String
lower = map toLower

makeNextMapping :: String -> Map.Map String (Set.Set String)
makeNextMapping str = foldr addWordToCorr Map.empty (clean str)

clean :: String -> [(String, String)]
clean input = concatMap (windows . stripAndSplit) (toLowerSentences input)

stripAndSplit :: String -> [String]
stripAndSplit = words . stripPunctuation

addWordToCorr :: (String, String) -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
addWordToCorr (f, s) acc = Map.unionWith Set.union acc (Map.fromList [(f, Set.singleton s)])

makePrevMapping :: String -> Map.Map String (Set.Set String)
makePrevMapping str = foldr (addWordToCorr . swap) Map.empty (clean str)

makePhrases :: String -> Set.Set [String]
makePhrases input = Set.fromList (phrases (concatMap stripAndSplit (toLowerSentences input)))

toLowerSentences :: String -> [String]
toLowerSentences input = splitOn "." (lower input)

phrases :: [String] -> [[String]]
phrases = concatMap (tail . inits) . tails