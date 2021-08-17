module Ortho (Ortho (..), eatWord, Ortho.size, Ortho.empty, insert, findWithMatchingDimsAndOriginProjectingForward, findWithMatchingDimsAndOriginProjectingBackward, findWithMatchingDimsAndHopToOrigin, findWithMatchingDimsAndOriginToHop) where
import Data.List (group, sort, groupBy, subsequences, nubBy, sortBy)
import Data.Set as Set
    ( Set,
      fromList,
      toList,
      size,
      Set,
      empty,
      fromList,
      singleton,
      toList,
      union,
      unions )
import Data.Map as Map (Map, singleton, fromList, empty, lookup, keys, (!), elems, size)
import WordEater ( Answer(..), wordToUniqueAnswer )
import Data.Function (on)
import Config
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)

newtype Ortho = Ortho{origin :: Node} deriving (Eq, Ord, Show)
data NodeName = Origin String | Hop String deriving (Show, Eq, Ord)
newtype Orthos = Orthos (Map.Map ([Int], NodeName) (Set.Set Ortho)) deriving (Show)
data Node = Node
  {
    name :: String,
    neighbors :: Map.Map Int Node
  } deriving (Eq, Ord, Show)

eatWord :: Config -> String -> [Ortho]
eatWord conf cur = fromAnswer <$> wordToUniqueAnswer conf cur

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = let
  nodeA = Node a (Map.fromList [(0, nodeB), (1, nodeC)])
  nodeB = Node b (Map.fromList [(1, nodeD)])
  nodeC = Node c (Map.fromList [(0, nodeD)])
  nodeD = Node d Map.empty
  in Ortho nodeA

getOrigin :: Ortho -> String
getOrigin = name . origin

getDimensions :: Ortho -> [Int]
getDimensions ortho = map length $ getDiagonals ortho

hop :: Ortho -> Set.Set String
hop (Ortho (Node _ neighbors)) = Set.fromList $ map name $ Map.elems neighbors

getDiagonals :: Ortho -> [Set.Set String]
getDiagonals ortho@(Ortho node) = let
  paths = getDiagonalPaths ortho
  zipped = zip (map length paths) (map (go node) paths)
  grouped = groupBy ((==) `on` fst) zipped
  in map (Set.fromList . map snd) grouped

getDiagonalPaths :: Ortho -> [[Int]]
getDiagonalPaths ortho = let
  pathToBottomRightCorner = findPathToBottomRightCorner ortho
  paths = subsequences pathToBottomRightCorner
  uniques = nubBy ((==) `on` sort) paths
  answer = sortBy (compare `on` length) uniques
  in answer

findPathToBottomRightCorner :: Ortho -> [Int]
findPathToBottomRightCorner (Ortho node@(Node _ neighbors)) = let
  axes = Map.keys neighbors
  in eatAxesToNode node axes

eatAxesToNode :: Node -> [Int] -> [Int]
eatAxesToNode node@(Node _ neighbors) [] = []
eatAxesToNode node@(Node _ neighbors) [direction] = replicate (distanceToEdge node direction) direction
eatAxesToNode node@(Node _ neighbors) (direction : rest) = let
  f = replicate (distanceToEdge node direction) direction
  s = eatAxesToNode node rest
  in f ++ s

distanceToEdge :: Node -> Int -> Int
distanceToEdge (Node _ neighbors) direction = case Map.lookup direction neighbors of
  Just node -> 1 + distanceToEdge node direction
  Nothing -> 0

go :: Node -> [Int] -> String
go (Node name _) [] = name
go (Node _ neighbors) (f : rest) = go (neighbors Map.! f) rest

empty :: Orthos
empty = Orthos Map.empty

insert :: Orthos -> Ortho -> Orthos
insert (Orthos orthoMap) ortho =
  let dims = getDimensions ortho
      origin = Origin $ getOrigin ortho
      originKey = (dims, origin, ortho)
      hopKeys = Hop <$> Set.toList (hop ortho)
      missingKeys = originKey : zip3 (repeat dims) hopKeys (repeat ortho)
      updatedMap = foldr addKeyToMap orthoMap missingKeys
   in Orthos updatedMap

size :: Orthos -> Int
size (Orthos m)= Map.size m

addKeyToMap :: ([Int], NodeName, Ortho) -> Map.Map ([Int], NodeName) (Set Ortho) -> Map.Map ([Int], NodeName) (Set Ortho)
addKeyToMap (dims, nodeName, ortho) = Map.insertWith Set.union (dims, nodeName) (Set.singleton ortho)

findWithMatchingDimsAndOriginProjectingForward :: Config -> Orthos -> Ortho -> Set.Set Ortho
findWithMatchingDimsAndOriginProjectingForward config = lookupHelper (next config)

findWithMatchingDimsAndOriginProjectingBackward :: Config -> Orthos -> Ortho -> Set.Set Ortho
findWithMatchingDimsAndOriginProjectingBackward config = lookupHelper (prev config)

lookupHelper :: Map.Map String (Set.Set String)  -> Orthos -> Ortho -> Set.Set Ortho
lookupHelper mapping (Orthos orthoMap) ortho = let
    projectedOrigin = Map.findWithDefault Set.empty (getOrigin ortho) mapping
    dims = getDimensions ortho
    originKeys = Origin <$> Set.toList projectedOrigin
    missingKeys = zip (repeat dims) originKeys
    results = (\key -> Map.findWithDefault Set.empty key orthoMap) <$> missingKeys
   in Set.unions results

findWithMatchingDimsAndHopToOrigin :: Orthos -> Ortho -> Set.Set Ortho
findWithMatchingDimsAndHopToOrigin (Orthos orthoMap) ortho =
  let dims = getDimensions ortho
      hops = Set.toList (hop ortho)
      hopKeys = Origin <$> hops
      missingKeys = zip (repeat dims) hopKeys
      results = (\key -> Map.findWithDefault Set.empty key orthoMap) <$> missingKeys
   in Set.unions results

findWithMatchingDimsAndOriginToHop :: Orthos -> Ortho -> Set.Set Ortho
findWithMatchingDimsAndOriginToHop (Orthos orthoMap) ortho =
  let dims = getDimensions ortho
      orthoOrigin = getOrigin ortho
      hopKeys = Hop orthoOrigin
      missingKeys = (dims, hopKeys)
      results = Map.findWithDefault Set.empty missingKeys orthoMap
   in results

checkDiagonals :: Ortho -> Ortho -> Bool
checkDiagonals l r = undefined

checkEachPositionProjects :: Config -> Ortho -> Ortho -> Bool
checkEachPositionProjects config l r = undefined

checkCentersMatch :: Ortho -> Ortho -> Bool
checkCentersMatch l r = undefined

checkPhrases :: Config -> Ortho -> Ortho -> Bool
checkPhrases config l r = undefined