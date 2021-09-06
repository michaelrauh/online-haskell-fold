module Ortho (Ortho (..)) where
import Data.Set as Set
import Data.Map as Map
import WordEater (Answer(..), wordToUniqueAnswer )
import Data.Function (on)
import Config
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)
import Data.List (delete, sort, maximumBy, nub, groupBy, permutations)
import Data.Text
import Data.Maybe

newtype Path = Path {path :: [Text]} deriving (Eq, Ord, Show)
data Node = Node
  { name :: Text,
    location :: Path
  } deriving (Eq, Ord, Show)
newtype Ortho = Ortho {nodes :: Set.Set Node} deriving (Eq, Ord)
newtype Orthos = Orthos (Set.Set Ortho)
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)
data DirectedOrtho = DirectedOrtho {ortho :: ShiftedOrtho, combineAxis :: Text}
newtype ShiftedOrtho = ShiftedOrtho Ortho
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(String, String)]}

digestWords :: Config -> Orthos
digestWords config = Prelude.foldr (digestWord config) emptyOrtho $ vocab config

digestWord :: Config -> String -> Orthos -> Orthos
digestWord config word orthos = sift config orthos $ eatWord config word

sift :: Config -> Orthos -> Orthos -> Orthos
sift config known increment = if isEmpty increment then known else let
  (f, rest) = pickOne increment
  incrementUp = combineUpIfBase config known f
  newKnown = mergeOrthos known incrementUp
  up = sift config newKnown incrementUp
  incrementOver = combineOver config up f
  veryNewKnown = mergeOrthos newKnown incrementOver
  over = sift config veryNewKnown incrementOver
  in sift config over rest

mergeOrthos :: Orthos -> Orthos -> Orthos
mergeOrthos (Orthos o1) (Orthos o2) = Orthos $ Set.union o1 o2

fromList :: [Ortho] -> Orthos -- export this and not the data constructor
fromList orthos = Orthos $ Set.fromList orthos

pickOne :: Orthos -> (Ortho, Orthos)
pickOne (Orthos o) = let
  (f, s) = Set.deleteFindMin o -- at this time min is arbitrary but if ord were defined by distance, then min would iterate in order of distance
  in (f, Orthos s)

isEmpty :: Orthos -> Bool
isEmpty (Orthos o)= Set.null o

eatWord :: Config -> String -> Orthos
eatWord conf cur = Orthos $ Set.fromList $ fromAnswer <$> wordToUniqueAnswer conf cur

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = Ortho $ Set.fromList
  [Node a' $ Path [],
   Node b' $ Path [b'],
   Node c' $ Path [c'],
   Node d' $ Path $ sort [b', c']] where
  a' = pack a
  b' = pack b
  c' = pack c
  d' = pack d

emptyOrtho :: Orthos
emptyOrtho = Orthos Set.empty

insert :: Orthos -> Ortho -> Orthos
insert (Orthos s) o = Orthos $ Set.insert o s

size :: Orthos -> Int
size (Orthos o) = Set.size o

combineUpIfBase :: Config -> Orthos -> Ortho -> Orthos
combineUpIfBase c os o = if isNotBase o then emptyOrtho else let
  l = combineUpLeft c os o
  r = combineUpRight c os o
  in mergeOrthos l r

-- this would be much faster if ortho were indexed by distance
isNotBase :: Ortho -> Bool
isNotBase (Ortho s) = let
  (Path underlying) = location $ maximumBy (compare `on` locationLength) $ Set.toList s
  in nub underlying /= underlying

locationLength :: Node -> Int
locationLength (Node _ (Path l)) = Prelude.length l

-- Reader would allow skipping this config pass
combineUpRight :: Config -> Orthos -> Ortho -> Orthos
combineUpRight c os o = let
  projects = projectsBackward c os o
  found = diagonalsLeft projects o
  in filterOld os found

filterOld :: Orthos -> Orthos -> Orthos
filterOld (Orthos old) (Orthos new) = Orthos $ new `Set.difference` old

combineUpLeft :: Config -> Orthos -> Ortho -> Orthos
combineUpLeft c os o = let
  projects = projectsForward c os o
  found = diagonalsRight projects o
  in filterOld os found

projectsForward :: Config -> Orthos -> Ortho -> Orthos
projectsForward c os o = let
  ns = nodes o
  origin = Set.findMin $ Set.filter ((0 ==) . locationLength) ns -- this would be faster if indexed by distance. Alternatively if ord were distance findMin would work. This would also expose toAscList
  forward = Map.findWithDefault Set.empty (unpack (name origin)) (next c) -- this unpack would be unneccesary if everything was text
  matchingOrigin = findWithOrigin os forward
  axisCorrespondence = Prelude.concat $ findAxisCorrespondence c o <$> Ortho.toList matchingOrigin
  in Orthos $ Set.fromList (toOrtho <$> Prelude.filter (checkAllProjectionsExceptOriginAndHop c) axisCorrespondence)

checkAllProjectionsExceptOriginAndHop :: Config -> Correspondence -> Bool -- reader would jump config over this
checkAllProjectionsExceptOriginAndHop c (Correspondence from to corr) = let
  corrMap = Map.fromList corr
  nodesToCheckFilter = Set.filter ((<) 2 . locationLength) . nodes -- this would be simpler with distance bucketing. Potentially slower though.
  nodesToCheckFrom = nodesToCheckFilter from
  nodesToCheckTo = nodesToCheckFilter to
  checksPassed = checkNodeProjectsForwardAcrossMappedPath c corrMap nodesToCheckTo nodesToCheckFrom
  in checksPassed

checkNodeProjectsForwardAcrossMappedPath :: Config -> Map String String -> Set Node -> Set Node -> Bool -- reader would jump config over this
checkNodeProjectsForwardAcrossMappedPath c corrMap from to = let
  correspondingNodeNames = findCorresponding corrMap to <$> Set.toList from
  in checkEachPairProjects c correspondingNodeNames

findCorresponding :: Map String String -> Set Node -> Node -> (String, String) 
findCorresponding corrMap toSet (Node fromName (Path fromLocation)) = let
  toPath = Path (pack . (corrMap !) . unpack <$> fromLocation)
  toNode = Set.findMin $ Set.filter ((toPath ==) . location) toSet -- this would be faster if there could be a mapping from path to name
  in (unpack fromName, (unpack . name) toNode)

findAxisCorrespondence :: Config -> Ortho -> Ortho -> [Correspondence] -- there are less redundant ways to do this. If one axis fails it's a complete failure.
findAxisCorrespondence c from to = let
  corrPerms = uncurry Prelude.zip <$> ((,) <$> permutations (Set.toList $ getHop from) <*> permutations (Set.toList $ getHop to))
  finds = Prelude.filter (checkEachPairProjects c) corrPerms
  in Correspondence from to <$> finds

checkEachPairProjects :: Config -> [(String, String)] -> Bool -- this would be faster if text
checkEachPairProjects  = undefined

getHop :: Ortho -> Set.Set String -- this would be faster if ord were distance, or if things were bucketed by distance
getHop = undefined

toList :: Orthos -> [Ortho]
toList (Orthos s) = Set.toList s

findWithOrigin :: Orthos -> Set String -> Orthos -- this would be faster if orthos were indexed by origin
findWithOrigin = undefined

projectsBackward :: Config -> Orthos -> Ortho -> Orthos
projectsBackward = undefined

diagonalsLeft :: Orthos -> Ortho -> Orthos
diagonalsLeft = undefined

diagonalsRight :: Orthos -> Ortho -> Orthos
diagonalsRight = undefined

combineOver :: Config -> Orthos -> Ortho -> Orthos
combineOver = undefined

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