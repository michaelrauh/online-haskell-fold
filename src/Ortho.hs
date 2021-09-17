module Ortho where
import Data.Set as Set ( filter, findMin, fromList, toList, Set, lookupMax, findMax, dropWhileAntitone, takeWhileAntitone, empty, deleteMin )
import Data.Map as Map ( (!), fromList, Map )
import WordEater (Answer(..) )
import Data.Function (on)
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)
import Data.List (delete, sort, maximumBy, nub, groupBy, permutations)
import Data.Text ( pack, unpack, Text )
import Data.Hashable ( Hashable(hashWithSalt) )
import Data.MultiSet as MultiSet

newtype Path = Path {path :: [MultiSet.MultiSet Text]} deriving (Eq, Ord, Show) -- change path to data multiset to skip sort when creating and comparing paths.
data Node = Node
  { name :: Text,
    location :: Path
  } deriving (Eq, Show)
newtype Ortho = Ortho {nodes :: Set.Set Node} deriving (Eq)
data DirectedOrtho = DirectedOrtho {ortho :: ShiftedOrtho, combineAxis :: Text}
newtype ShiftedOrtho = ShiftedOrtho Ortho
newtype Dims = Dims [MultiSet.MultiSet Int] deriving (Eq, Ord) -- change this to Data.Multiset to make it clear and get good ord behavior

instance Ord Ortho where
  compare a b = let
    dimsComp = (compare `on` getDims) a b
    originComp = (compare `on` getOrigin) a b
    in if dimsComp /= EQ then dimsComp else originComp

instance Ord Node where
  compare = compare `on` length . path . location

getDims :: Ortho -> Dims
getDims = undefined

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

isNotBase :: Ortho -> Bool
isNotBase (Ortho s) = let
  underlying = (path . location . Set.findMax) s
  in nub underlying /= underlying

locationLength :: Node -> Int
locationLength (Node _ (Path l)) = Prelude.length l

getName :: Node -> Text
getName = name

getOrigin :: Ortho -> Node
getOrigin = Set.findMin . nodes

getNonOriginAndHopNodes :: Ortho -> Set.Set Node
getNonOriginAndHopNodes (Ortho nodes) = Set.dropWhileAntitone ((< 1) . locationLength) nodes

findCorresponding :: Map String String -> Set Node -> Node -> (String, String)
findCorresponding corrMap toSet fromNode = let
  toPath = Path (pack . (corrMap !) . unpack <$> getLocation fromNode)
  toNode = findNodeWithPath toPath toSet
  in (unpack (getName fromNode), (unpack . name) toNode)

getLocation :: Node -> [Text]
getLocation = path . location

findNodeWithPath :: Path -> Set Node -> Node
findNodeWithPath toPath toSet = let
  distance = (length . path) toPath
  eliminatedFirst = Set.dropWhileAntitone ((< distance) . length . path . location) toSet
  eliminatedSecond = Set.takeWhileAntitone ((== distance) . length . path . location) eliminatedFirst
  in Set.findMin $ Set.filter ((toPath ==) . location) eliminatedSecond

getHop :: Ortho -> Set.Set String
getHop (Ortho s) = let
  allButOrigin = Set.deleteMin s
  hopSet = Set.takeWhileAntitone ((== 1) . length . path . location) allButOrigin
  in Set.fromList $ unpack . getName <$> Set.toList hopSet