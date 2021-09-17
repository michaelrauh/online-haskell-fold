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

data Orthos = Full FullOrthos | Sub SubOrthos
data SubOrthos = SubOrthos Origins Hops
data FullOrthos = FullOrthos Origins Hops
newtype Origins = Origins (Set.Set Ortho)
newtype Hops = Hops (Set.Set FocusedHop)
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(Text, Text)]}
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)
data FocusedHop = FocusedHop Text Ortho deriving (Eq)

instance Ord FocusedHop where 
  compare (FocusedHop t1 o1) (FocusedHop t2 o2) = undefined -- let
    -- dimsComp = (compare `on` getDims) o1 o2
    -- focusComp = compare t1 t2
    -- in if dimsComp /= EQ then dimsComp else focusComp

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

selectByDims :: Orthos -> Dims -> Orthos -- this cannot be called after selectByHop or selectByOrigin. express this by returning suborthos. Consider exposing this to the caller.
selectByDims = undefined 

selectByHop :: Orthos -> Text -> Orthos 
selectByHop = undefined 

selectByOrigin :: Orthos -> Text -> Orthos 
selectByOrigin = undefined 

fromSet :: Set.Set Orthos -> Orthos 
fromSet = undefined