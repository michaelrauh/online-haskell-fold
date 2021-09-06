module Ortho where
import Data.Set as Set ( filter, findMin, fromList, toList, Set )
import Data.Map as Map ( (!), fromList, Map )
import WordEater (Answer(..) )
import Data.Function (on)
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)
import Data.List (delete, sort, maximumBy, nub, groupBy, permutations)
import Data.Text ( pack, unpack, Text )

-- TODO make node ord be distance
-- TODO wrap ortho in square or rectangle. up returns input if rectangle and always returns a square. Over returns rectangle.
-- TODO export selectively from here
-- TODO use Text

newtype Path = Path {path :: [Text]} deriving (Eq, Ord, Show)
data Node = Node
  { name :: Text,
    location :: Path
  } deriving (Eq, Ord, Show)
newtype Ortho = Ortho {nodes :: Set.Set Node} deriving (Eq, Ord)
data DirectedOrtho = DirectedOrtho {ortho :: ShiftedOrtho, combineAxis :: Text}
newtype ShiftedOrtho = ShiftedOrtho Ortho

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

-- this would be much faster if ortho were indexed by distance. lookupMax would do it. alternatively inspect a mapping from axis name to length
isNotBase :: Ortho -> Bool
isNotBase (Ortho s) = let
  (Path underlying) = location $ maximumBy (compare `on` locationLength) $ Set.toList s
  in nub underlying /= underlying

locationLength :: Node -> Int
locationLength (Node _ (Path l)) = Prelude.length l

getName :: Node -> Text
getName = name

getOrigin :: Ortho -> Node
getOrigin o = Set.findMin $ Set.filter ((0 ==) . locationLength) (nodes o) -- this would be faster if indexed by distance. Alternatively if ord were distance findMin would work

getNodesOfDistanceGreaterThan :: Int -> Ortho -> Set.Set Node
getNodesOfDistanceGreaterThan dist (Ortho nodes) = Set.filter ((> 1) . locationLength) nodes -- this would be simpler and faster with distance bucketing

findCorresponding :: Map String String -> Set Node -> Node -> (String, String) 
findCorresponding corrMap toSet fromNode = let
  toPath = Path (pack . (corrMap !) . unpack <$> getLocation fromNode)
  toNode = findNodeWithPath toPath toSet
  in (unpack (getName fromNode), (unpack . name) toNode)

getLocation :: Node -> [Text]
getLocation = error "not implemented"

findNodeWithPath :: Path -> Set Node -> Node -- this would be faster if indexed by distance. Filter by size first then check path.
findNodeWithPath toPath toSet = Set.findMin $ Set.filter ((toPath ==) . location) toSet

getHop :: Ortho -> Set.Set String -- this would be faster if ord were distance, or if things were bucketed by distance
getHop = undefined