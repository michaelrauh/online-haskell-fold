module Config (Config (Config, next, prev, phrases, vocab), makeConfig) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import ConfigUtil
    (makeNextMapping, makePrevMapping, makePhrases, makeVocabulary)
import Data.Text (Text)

data Config = Config {
    next :: Map.Map Text (Set.Set Text),
    prev :: Map.Map Text (Set.Set Text),
    phrases :: Set.Set [Text],
    vocab :: [Text]
} deriving Show

makeConfig :: Text -> Config
makeConfig s = Config {next= makeNextMapping s, prev=makePrevMapping s, phrases=makePhrases s, vocab=makeVocabulary s}  