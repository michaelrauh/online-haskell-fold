module Ortho (Ortho (..), eatWord, Ortho.size, Ortho.empty, insert, findWithMatchingOriginProjectingForward, findWithMatchingOriginProjectingBackward, findWithMatchingHopToOrigin, findWithMatchingOriginToHop) where
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
newtype Orthos = Orthos (Map.Map NodeName (Set.Set Ortho)) deriving (Show)
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

hop :: Ortho -> Set.Set String
hop (Ortho (Node _ neighbors)) = Set.fromList $ map name $ Map.elems neighbors

empty :: Orthos
empty = Orthos Map.empty

insert :: Orthos -> Ortho -> Orthos
insert (Orthos orthoMap) ortho =
  let origin = Origin $ getOrigin ortho
      originKey = (origin, ortho)
      hopKeys = Hop <$> Set.toList (hop ortho)
      missingKeys = originKey : zip hopKeys (repeat ortho)
      updatedMap = foldr (\(nn, o) -> Map.insertWith Set.union nn (Set.singleton o)) orthoMap missingKeys
   in Orthos updatedMap

size :: Orthos -> Int
size (Orthos m)= Map.size m

findWithMatchingOriginProjectingForward :: Config -> Orthos -> Ortho -> Set.Set Ortho
findWithMatchingOriginProjectingForward config = lookupHelper (next config)

findWithMatchingOriginProjectingBackward :: Config -> Orthos -> Ortho -> Set.Set Ortho
findWithMatchingOriginProjectingBackward config = lookupHelper (prev config)

lookupHelper :: Map.Map String (Set.Set String)  -> Orthos -> Ortho -> Set.Set Ortho
lookupHelper mapping (Orthos orthoMap) ortho = let
    projectedOrigin = Map.findWithDefault Set.empty (getOrigin ortho) mapping
    originKeys = Origin <$> Set.toList projectedOrigin
    results = (\key -> Map.findWithDefault Set.empty key orthoMap) <$> originKeys
   in Set.unions results

findWithMatchingHopToOrigin :: Orthos -> Ortho -> Set.Set Ortho
findWithMatchingHopToOrigin (Orthos orthoMap) ortho =
  let hops = Set.toList (hop ortho)
      hopKeys = Origin <$> hops
      results = (\key -> Map.findWithDefault Set.empty key orthoMap) <$> hopKeys
   in Set.unions results

findWithMatchingOriginToHop :: Orthos -> Ortho -> Set.Set Ortho
findWithMatchingOriginToHop (Orthos orthoMap) ortho =
  let orthoOrigin = getOrigin ortho
      hopKeys = Hop orthoOrigin
   in Map.findWithDefault Set.empty hopKeys orthoMap

checkDiagonals :: Ortho -> Ortho -> Bool
checkDiagonals l r = undefined

checkEachPositionProjectsForward :: Config -> Ortho -> Ortho -> Bool
checkEachPositionProjectsForward config l r = undefined

checkEachPositionProjectsBackward :: Config -> Ortho -> Ortho -> Bool
checkEachPositionProjectsBackward config l r = undefined

checkCentersMatch :: Ortho -> Ortho -> Bool
checkCentersMatch l r = undefined

checkPhrases :: Config -> Ortho -> Ortho -> Bool
checkPhrases config l r = undefined