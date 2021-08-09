module Ortho (eatWord, getOrigin, getDimensions, hop, unravel, diagonals) where

import Config (Config (Config))
import Data.List (group, sort, groupBy)
import Data.Set as Set (Set, fromList, toList)
import Data.Map as Map (Map, singleton, fromList, empty, lookup, keys)
import WordEater ( Answer(..), wordToUniqueAnswer )
import Data.List.NonEmpty (cons)

newtype Ortho = Ortho{origin :: Node}

data Node = Node
  { name :: String,
    distance :: Int,
    unravelNeighbor :: Map.Map String Node
  }

eatWord :: Config -> String -> [Ortho]
eatWord conf cur = fromAnswer <$> wordToUniqueAnswer conf cur

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = let
  nodeA = Node a 0 (Map.fromList [(b, nodeB), (c, nodeC)])
  nodeB = Node b 1 (Map.fromList [(b, nodeC), (c, nodeD)])
  nodeC = Node c 1 (Map.fromList [(b, nodeD), (c, nodeB)])
  nodeD = Node d 2 Map.empty
  in Ortho nodeA

getOrigin :: Ortho -> String
getOrigin = name . origin

getDimensions :: Ortho -> [Int]
getDimensions (Ortho origin) = let
  hopDirection = (head . Map.keys . unravelNeighbor) origin
  numbers = snd <$> go origin hopDirection
  in map length $ group $ sort numbers

hop :: Ortho -> Set.Set String
hop (Ortho (Node _ _ neighbor)) = Set.fromList $ Map.keys neighbor

-- isBase :: Ortho -> Bool
-- isBase = error "Not Implemented"

unravel :: Ortho -> String -> [String]
unravel (Ortho origin) hopDirection = fst <$> go origin hopDirection

go :: Node -> String -> [(String, Int)]
go (Node name distance neighbors) hopDirection =
  case Map.lookup hopDirection neighbors of
    Nothing -> [(name, distance)]
    Just node -> (name, distance) : go node hopDirection

diagonals :: Ortho -> [Set.Set String]
diagonals (Ortho origin) = let
  hopDirection = (head . Map.keys . unravelNeighbor) origin
  pairs = go origin hopDirection
  in map (Set.fromList . map fst) (groupBy ((. snd) . (==) . snd) pairs)

-- lhsCenter :: Ortho -> String -> [String]
-- lhsCenter = error "Not Implemented"

-- rhsCenter :: Ortho -> String -> [String]
-- rhsCenter = error "Not Implemented"

-- phrases :: Ortho -> [[String]]
-- phrases = error "Not Implemented"