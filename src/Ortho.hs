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
data DirectedOrtho = DirectedOrtho {ortho :: ShiftedOrtho, combineAxis :: Text}
newtype ShiftedOrtho = ShiftedOrtho Ortho

digestWords :: Config -> Orthos
digestWords config = Prelude.foldr (digestIt config) emptyOrtho $ vocab config

digestIt :: Config -> String -> Orthos -> Orthos
digestIt config word orthos = digestWord config orthos $ pack word

digestWord :: Config -> Orthos -> Text -> Orthos
digestWord config orthos word = sift config orthos $ eatWord config word

sift :: Config -> Orthos -> Orthos -> Orthos
sift config known increment = if emptyOrthos increment then known else let 
  (f, rest) = pickOne increment
  incrementUp = combineIfBase config known f
  newKnown = mergeOrthos known incrementUp
  up = sift config newKnown incrementUp
  incrementOver = combineOver config up f
  veryNewKnown = mergeOrthos newKnown incrementOver
  over = sift config veryNewKnown incrementOver
  in sift config over rest

mergeOrthos :: Orthos -> Orthos -> Orthos
mergeOrthos = undefined

combineOver :: Config -> Orthos -> Ortho -> Orthos
combineOver = undefined

pickOne :: Orthos -> (Ortho, Orthos)
pickOne = undefined

emptyOrthos :: Orthos -> Bool 
emptyOrthos = undefined

combineIfBase :: Config -> Orthos -> Ortho -> Orthos -- if it is not base return the empty orthos. If it is base return only previously unknown results
combineIfBase = error "not implemented"

eatWord :: Config -> Text -> Orthos
eatWord = undefined

fromAnswer :: Answer -> Ortho
fromAnswer = undefined

emptyOrtho :: Orthos
emptyOrtho = undefined

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