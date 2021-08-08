module Ortho (eatWord, origin, dimensions, hop) where

import Config (Config (Config))
import Data.List (group, sort)
import Data.Set as Set (Set, fromList)
import WordEater ( Answer(..), wordToUniqueAnswer )

newtype Ortho = Ortho {nodes :: [Node]} -- todo add entry points list that index into nodes. Rename nodes getNodes

data Node = Node
  { name :: String,
    distance :: Integer
  }

eatWord :: Config -> String -> [Ortho]
eatWord conf cur = map fromAnswer $ wordToUniqueAnswer conf cur

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = Ortho [Node a 0, Node b 1, Node c 1, Node d 2]

origin :: Ortho -> String
origin = name . head . nodes

dimensions :: Ortho -> [Int]
dimensions (Ortho nodes) = map length $ group $ sort $ map distance nodes

hop :: Ortho -> Set.Set String
hop (Ortho nodes) = Set.fromList $ map name $ filter ((1 ==) . distance) nodes

isBase :: Ortho -> Bool 
isBase = error "Not Implemented"

unravel :: Ortho -> String -> [String]
unravel = error "Not Implemented"

diagonals :: Ortho -> [Set.Set String]
diagonals = error "Not Implemented"

lhsCenter :: Ortho -> String -> [String]
lhsCenter = error "Not Implemented"

rhsCenter :: Ortho -> String -> [String]
rhsCenter = error "Not Implemented"

phrases :: Ortho -> [[String]]
phrases = error "Not Implemented"