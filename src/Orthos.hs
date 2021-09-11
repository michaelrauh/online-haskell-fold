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

type DimsMapping = Map.Map Dims (Map.Map Text Ortho)
data Orthos = Orthos Origins Hops
newtype Origins = Origins DimsMapping
newtype Hops = Hops DimsMapping
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(String, String)]}
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)

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

-- two approaches : keep it all map of map, or have selectByDims return a map, and selectByHop return a set.
-- Then the other functions can support maps of maps, maps, or sets for speed.