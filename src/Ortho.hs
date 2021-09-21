module Ortho where
import Data.Set as Set ( filter, findMin, fromList, toList, Set, lookupMax, findMax, dropWhileAntitone, takeWhileAntitone, empty, deleteMin, toAscList, map, union )
import Data.Map as Map ( (!), fromList, Map )
import WordEater (Answer(..) )
import Data.Function (on)
import qualified Data.Map as Map (Map, empty, findWithDefault, insertWith)
import Data.List (delete, sort, maximumBy, nub, groupBy, permutations)
import Data.Text ( Text )
import Data.Hashable ( Hashable(hashWithSalt) )
import Data.MultiSet as MultiSet
    ( distinctSize, empty, fromList, map, singleton, size, MultiSet, toList, insert )

newtype Path = Path {path :: MultiSet.MultiSet Text} deriving (Eq, Ord, Show)
data Node = Node
  { name :: Text,
    location :: Path
  } deriving (Eq, Show)
newtype Ortho = Ortho {nodes :: Set.Set Node} deriving (Eq, Show)
data DirectedOrtho = DirectedOrtho {ortho :: ShiftedOrtho, combineAxis :: Text}
newtype ShiftedOrtho = ShiftedOrtho Ortho
newtype Dims = Dims (MultiSet.MultiSet Int) deriving (Eq, Ord)
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: Map.Map Text Text}

makePretty :: Ortho -> String
makePretty (Ortho o)= show ((fmap . fmap) name (groupBy ((==) `on` locationLength) (Set.toAscList o)))

mergeUp :: Correspondence -> Ortho
mergeUp (Correspondence (Ortho f) to@(Ortho t) corr) = let
  mappedTo = Set.map (mapPaths corr) t
  pushedBack = Set.map (insertIntoPath (name $ getOrigin to)) t
  in Ortho $ Set.union pushedBack f

insertIntoPath :: Text -> Node -> Node
insertIntoPath toInsert (Node name (Path l))= Node name $ Path $ MultiSet.insert name l

mapPaths :: Map Text Text -> Node -> Node
mapPaths corr (Node name (Path location)) = let 
  locations = MultiSet.map (corr Map.!) location
  in Node name (Path locations) 

instance Ord Ortho where
  compare a b = undefined -- let
    -- dimsComp = (compare `on` getDims) a b
    -- originComp = (compare `on` getOrigin) a b
    -- in if dimsComp /= EQ then dimsComp else originComp

instance Ord Node where
  compare a b = let
    distComp = (compare `on` length . path . location) a b
    nameComp = (compare `on` name) a b
    in if distComp /= EQ then distComp else nameComp

getDims :: Ortho -> Dims
getDims = undefined

fromAnswer :: Answer -> Ortho
fromAnswer (Answer a b c d) = Ortho $ Set.fromList
  [Node a $ Path MultiSet.empty,
   Node b $ Path $ MultiSet.singleton b,
   Node c $ Path $ MultiSet.singleton c,
   Node d $ Path $ MultiSet.fromList [b, c]]

isNotBase :: Ortho -> Bool
isNotBase (Ortho s) = undefined -- let
  -- underlying = (path . location . Set.findMax) s
  -- in MultiSet.distinctSize underlying /= MultiSet.size underlying

locationLength :: Node -> Int
locationLength (Node _ (Path l)) = length . MultiSet.toList $ l

getName :: Node -> Text
getName = name

getOrigin :: Ortho -> Node
getOrigin = undefined -- Set.findMin . nodes

getNonOriginAndHopNodes :: Ortho -> Set.Set Node
getNonOriginAndHopNodes (Ortho nodes) = undefined -- Set.dropWhileAntitone ((< 1) . locationLength) nodes

findCorresponding :: Map Text Text -> Set Node -> Node -> (Text, Text)
findCorresponding corrMap toSet fromNode = undefined -- let
  -- toPath = Path $  MultiSet.map (corrMap !) (getLocation fromNode)
  -- toNode = findNodeWithPath toPath toSet
  -- in (getName fromNode, name toNode)

getLocation :: Node -> MultiSet Text
getLocation = undefined -- path . location

findNodeWithPath :: Path -> Set Node -> Node
findNodeWithPath toPath toSet = undefined -- let
  -- distance = (length . path) toPath
  -- eliminatedFirst = Set.dropWhileAntitone ((< distance) . length . path . location) toSet
  -- eliminatedSecond = Set.takeWhileAntitone ((== distance) . length . path . location) eliminatedFirst
  -- in Set.findMin $ Set.filter ((toPath ==) . location) eliminatedSecond

getHop :: Ortho -> Set.Set Text
getHop (Ortho s) = undefined -- let
  -- allButOrigin = Set.deleteMin s
  -- hopSet = Set.takeWhileAntitone ((== 1) . length . path . location) allButOrigin
  -- in Set.fromList $ getName <$> Set.toList hopSet