module Orthos where
import Ortho
    (
      Ortho,
      fromAnswer,
      isNotBase,
      getOrigin,
      getName, getNonOriginAndHopNodes, getHop, DirectedOrtho, getDims, Dims )
import Data.Set as Set
    ( deleteFindMin,
      difference,
      empty,
      fromList,
      insert,
      null,
      size,
      toList,
      union,
      Set, filter )
import Data.Map as Map ( findWithDefault, fromList, Map, (!) )
import Data.Text ( unpack, Text )
import WordEater (Answer (..), wordToUniqueAnswer)
import Data.List ( permutations )
import Data.Function ( on )

data Orthos = Orthos Origins Hops
newtype Origins = Origins (Set.Set Ortho)
newtype Hops = Hops (Set.Set FocusedHop)
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(String, String)]}
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)
data FocusedHop = FocusedHop Text Ortho deriving (Eq)

instance Ord FocusedHop where 
  compare (FocusedHop t1 o1) (FocusedHop t2 o2) = let
    dimsComp = (compare `on` getDims) o1 o2
    focusComp = compare t1 t2
    in if dimsComp /= EQ then dimsComp else focusComp

takeContiguousSubset :: (a -> Bool) -> Set.Set a -> Set.Set a
takeContiguousSubset = undefined

mergeOrthos :: Orthos -> Orthos -> Orthos
mergeOrthos = undefined

fromList :: [Ortho] -> Orthos
fromList = undefined

pickOne :: Orthos -> (Ortho, Orthos)
pickOne = undefined

isEmpty :: Orthos -> Bool
isEmpty = undefined

emptyOrtho :: Orthos
emptyOrtho = undefined

insert :: Orthos -> Ortho -> Orthos
insert = undefined

filterOld :: Orthos -> Orthos -> Orthos
filterOld = undefined

toList :: Orthos -> [Ortho]
toList = undefined

selectByDims :: Orthos -> Dims -> Orthos
selectByDims = undefined 

selectByHop :: Orthos -> Text -> Orthos 
selectByHop = undefined 

selectByOrigin :: Orthos -> Text -> Orthos
selectByOrigin = undefined 

fromSet :: Set.Set Orthos -> Orthos 
fromSet = undefined

-- two approaches : keep it all map of map, or have selectByDims return a map, and selectByHop and selectByOrigin return a set.
-- Then the other functions can support maps of maps, maps, or sets for speed.
-- alternatively, use sets for everything. If ord is by dims and then by origin or hop, antitone functions will give the desired output on selects.
-- this could be two sets or one set for hop and origin. selectByOrigin implies no hops.
-- todo make a wrapper newtype for ortho that is origin or hop. Then make an ord instance for each that is (dims, text) where text is origin or hop
-- issue - hop is one to many. either ord by latest hop, or do a straight linear filter for those.
-- actual plan: ord by dims and origin. If selecting by dims or by origin, use antitone (make select by origin safe by dropping by dims first). 
-- wrap hop orthos in a type that adds a focus. Generate one of these to focus on each hop value. Ord by dims and focus. Then you get a lumpy tree with lots of repeats.
-- antitone will group them by dims and then by focus hop.