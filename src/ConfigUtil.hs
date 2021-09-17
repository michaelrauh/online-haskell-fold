module ConfigUtil (makeNextMapping, makePrevMapping, makePhrases, makeVocabulary) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (tails, inits, nub, sort)
import Data.Char (toLower)
import Data.Tuple (swap)
import Data.Text ( pack, unpack, Text, filter, map, words, splitOn )

makeNextMapping :: Text -> Map.Map Text (Set.Set Text)
makeNextMapping str = foldr addWordToCorr Map.empty (clean str)

makePrevMapping :: Text -> Map.Map Text (Set.Set Text)
makePrevMapping str = foldr (addWordToCorr . swap) Map.empty (clean str)

makePhrases :: Text -> Set.Set [Text]
makePhrases input = Set.fromList $ concatMap (phrases . stripAndSplit) (toLowerSentences input)

makeVocabulary :: Text -> [Text]
makeVocabulary = sort . nub . stripAndSplit . lower

windows :: [Text] -> [(Text, Text)]
windows x = zip x (drop 1 x)

stripPunctuation :: Text -> Text
stripPunctuation = Data.Text.filter (`notElem` ",.?!-:;\"\'")

lower :: Text -> Text
lower = Data.Text.map toLower

clean :: Text -> [(Text, Text)]
clean input = concatMap (windows . stripAndSplit) (toLowerSentences input)

stripAndSplit :: Text -> [Text]
stripAndSplit = Data.Text.words . stripPunctuation

addWordToCorr :: (Text, Text) -> Map.Map Text (Set.Set Text) -> Map.Map Text (Set.Set Text)
addWordToCorr (f, s) acc = Map.unionWith Set.union acc (Map.fromList [(f, Set.singleton s)])

toLowerSentences :: Text -> [Text]
toLowerSentences input = Data.Text.splitOn (pack ".") (lower input)

phrases :: [Text] -> [[Text]]
phrases = concatMap (tail . inits) . tails