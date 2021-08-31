{-# LANGUAGE OverloadedStrings #-}

module Ortho (Ortho (..)) where
import Data.Set as Set
import Data.Map as Map
import WordEater (Answer(..), wordToUniqueAnswer )
import Data.Function (on)
import Config
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)
import Data.List (delete)
import Data.Text

newtype Path = Path [Text] deriving (Eq, Ord, Show)
data Node = Node
  { name :: Text,
    location :: Path
  } deriving (Eq, Ord, Show)
newtype Ortho = Ortho {nodes :: Set.Set Node}
newtype Orthos = Orthos (Set.Set Ortho)
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)
data DirectedOrtho = DirectedOrtho {ortho :: Ortho, combineAxis :: Text}

x :: String 
x = "bar"

foo :: Text 
foo = pack x

eatWord :: Config -> Text -> Orthos
eatWord = undefined

fromAnswer :: Answer -> Ortho
fromAnswer = undefined

empty :: Orthos
empty = undefined

insert :: Orthos -> Ortho -> Orthos
insert = undefined

size :: Orthos -> Int
size = undefined

projectsForward :: Config -> Orthos -> Ortho -> Orthos
projectsForward = undefined

projectsBackward :: Config -> Orthos -> Ortho -> Orthos
projectsBackward = undefined

diagonalsLeft :: Orthos -> Ortho -> Orthos
diagonalsLeft = undefined

diagonalsRight :: Orthos -> Ortho -> Orthos
diagonalsRight = undefined

combineUpLeft :: Ortho -> Ortho -> Ortho
combineUpLeft = undefined

combineUpRight :: Ortho -> Ortho -> Ortho
combineUpRight = undefined

centersMatchLeft :: Orthos -> Ortho -> DirectedOrthos
centersMatchLeft = undefined

centersMatchRight :: Orthos -> Ortho -> DirectedOrthos
centersMatchRight = undefined

phrasesMatchLeft :: DirectedOrthos -> Ortho -> DirectedOrthos
phrasesMatchLeft = undefined

phrasesMatchRight :: DirectedOrthos -> Ortho -> DirectedOrthos
phrasesMatchRight = undefined

combineOverLeft :: DirectedOrthos -> Ortho -> Orthos
combineOverLeft = undefined

combineOverRight :: DirectedOrthos -> Ortho -> Orthos
combineOverRight = undefined