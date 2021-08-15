module Config (Config (Config, next, prev, phrases, vocab), makeConfig) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import ConfigUtil
    (makeNextMapping, makePrevMapping, makePhrases, makeVocabulary)

data Config = Config {
    next :: Map.Map String (Set.Set String),
    prev :: Map.Map String (Set.Set String),
    phrases :: Set.Set [String],
    vocab :: [String]
} deriving Show

makeConfig :: String -> Config
makeConfig s = Config {next=makeNextMapping s, prev=makePrevMapping s, phrases=makePhrases s, vocab=makeVocabulary s}  