module Ortho (eatWord, getOrigin, getDimensions, hop, unravel, diagonals, lhsCenter, rhsCenter) where

import Config (Config (Config))
import Data.List (group, sort, groupBy)
import Data.Set as Set (Set, fromList, toList)
import Data.Map as Map (Map, singleton, fromList, empty, lookup, keys, (!))
import WordEater ( Answer(..), wordToUniqueAnswer )

newtype Ortho = Ortho {origin :: Node}

data Node = Node
  { name :: String,
    distance :: Int,
    unravelNeighbor :: Map.Map String Node,
    lhsNeighbor :: Map.Map String Node,
    rhsNeighbor :: Map.Map String Node
  }

eatWord :: Config -> String -> [Ortho]
eatWord conf cur = fromAnswer <$> wordToUniqueAnswer conf cur

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = let
  nodeA = Node a 0 (Map.fromList [(b, nodeB), (c, nodeC)]) (Map.fromList [(b, nodeC), (c, nodeB)]) Map.empty
  nodeB = Node b 1 (Map.fromList [(b, nodeC), (c, nodeD)]) Map.empty (Map.fromList [(b, nodeD)])
  nodeC = Node c 1 (Map.fromList [(b, nodeD), (c, nodeB)]) Map.empty (Map.fromList [(c, nodeD)])
  nodeD = Node d 2 Map.empty Map.empty Map.empty
  in Ortho nodeA

getOrigin :: Ortho -> String
getOrigin = name . origin

getDimensions :: Ortho -> [Int]
getDimensions (Ortho origin) = let
  hopDirection = randomHop origin
  numbers = snd <$> go origin hopDirection
  in map length $ group $ sort numbers

randomHop :: Node -> String
randomHop = head . Map.keys . unravelNeighbor

hop :: Ortho -> Set.Set String
hop (Ortho (Node _ _ neighbor _ _)) = Set.fromList $ Map.keys neighbor

-- isBase :: Ortho -> Bool
-- isBase = error "Not Implemented"

unravel :: Ortho -> String -> [String]
unravel (Ortho origin) hopDirection = fst <$> go origin hopDirection

go :: Node -> String -> [(String, Int)]
go (Node name distance neighbors _ _) hopDirection =
  case Map.lookup hopDirection neighbors of
    Nothing -> [(name, distance)]
    Just node -> (name, distance) : go node hopDirection

goLHS :: Node -> String -> [(String, Int)]
goLHS (Node name distance _ neighbors _) hopDirection =
  case Map.lookup hopDirection neighbors of
    Nothing -> [(name, distance)]
    Just node -> (name, distance) : goLHS node hopDirection

goRHS :: Node -> String -> [(String, Int)]
goRHS (Node name distance _ _ neighbors) hopDirection =
  case Map.lookup hopDirection neighbors of
    Nothing -> [(name, distance)]
    Just node -> (name, distance) : goRHS node hopDirection

diagonals :: Ortho -> [Set.Set String]
diagonals (Ortho origin) = let
  hopDirection = randomHop origin
  pairs = go origin hopDirection
  in map (Set.fromList . map fst) (groupBy ((. snd) . (==) . snd) pairs)

lhsCenter :: Ortho -> String -> [String]
lhsCenter (Ortho origin) hopDirection = fst <$> goLHS origin hopDirection

rhsCenter :: Ortho -> String -> [String]
rhsCenter (Ortho (Node _ _ neighbor _ _)) hopDirection = let
  start = neighbor Map.! hopDirection
  in fst <$> goRHS start hopDirection

-- phrases :: Ortho -> [[String]]
-- phrases = error "Not Implemented"