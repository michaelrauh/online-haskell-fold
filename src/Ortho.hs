module Ortho (Ortho (..), eatWord, fromAnswer, getOrigin, getDimensions, hop, getDiagonals) where

import Config (Config (Config))
import Data.List (group, sort, groupBy, subsequences, nubBy, sortBy)
import Data.Set as Set (Set, fromList, toList, size)
import Data.Map as Map (Map, singleton, fromList, empty, lookup, keys, (!), elems)
import WordEater ( Answer(..), wordToUniqueAnswer )
import Data.Function (on)

newtype Ortho = Ortho{origin :: Node}

data Node = Node
  {
    name :: String,
    neighbors :: Map.Map Int Node
  }

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
  uniques = nubBy (\x y -> sort x == sort y) paths
  comparator = (compare `on` length)
  answer = sortBy comparator uniques
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