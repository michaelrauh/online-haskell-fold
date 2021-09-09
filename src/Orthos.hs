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
import Data.Map as Map ( findWithDefault, fromList )
import Data.Text ( unpack, Text )
import WordEater (Answer (..), wordToUniqueAnswer)
import Data.List ( permutations )

newtype Orthos = Orthos (Set.Set Ortho)
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(String, String)]}
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)

mergeOrthos :: Orthos -> Orthos -> Orthos
mergeOrthos (Orthos o1) (Orthos o2) = Orthos $ Set.union o1 o2

fromList :: [Ortho] -> Orthos
fromList orthos = Orthos $ Set.fromList orthos

pickOne :: Orthos -> (Ortho, Orthos)
pickOne (Orthos o) =
  let (f, s) = Set.deleteFindMin o
  in (f, Orthos s)

isEmpty :: Orthos -> Bool
isEmpty (Orthos o) = Set.null o

emptyOrtho :: Orthos
emptyOrtho = Orthos Set.empty

insert :: Orthos -> Ortho -> Orthos
insert (Orthos s) o = Orthos $ Set.insert o s

size :: Orthos -> Int
size (Orthos o) = Set.size o

selectSameDimensionalityForBaseOrtho :: Orthos -> Ortho -> Orthos -- revisit for indexing
selectSameDimensionalityForBaseOrtho (Orthos s) o = Orthos $ Set.filter ((getDims o ==) . getDims) s

filterOld :: Orthos -> Orthos -> Orthos
filterOld (Orthos old) (Orthos new) = Orthos $ new `Set.difference` old

toList :: Orthos -> [Ortho]
toList (Orthos s) = Set.toList s

findWithOrigin :: Orthos -> Set String -> Orthos -- this would be faster if orthos were indexed by origin
findWithOrigin = undefined

-- todo make mapping from ortho hash to ortho, and one from dims to set of hash, and one from origin to set of hash

selectByDimsAndHop :: Orthos -> Dims -> Text -> Orthos
selectByDimsAndHop = undefined 

selectByDimsAndOrigin :: Orthos -> Dims -> Text -> Orthos
selectByDimsAndOrigin = undefined

-- access pattern is that selectByDimsAndOrigin will be called once per thing. As currently written there is no reuse of intermediates but there is a refactor
-- where dims is pulled first, and that set is queried by origin several times for that increment.
-- access pattern for selectByDimsAndHop is that dims will be the same for many calls in a row. 

-- goals - 
-- it should not duplicate the orthos
-- it should not be slow
-- it should treat the two use cases as first class
-- ideally it should be amenable to data reuse such as narrowing to dims first
-- the above signatures are not ideal. There is a division point here. Extra flexibility can be real or implied by allowing any order of narrowing.
-- alternatively exposing a different type will give a feel for how this thing is used, and also prevent illusory speedups from currying that 
-- are not realized. To have it both ways use a let over lambda pattern. The question is whether a function type is prettier than an ADT

-- possibilities
-- 1. make map from hash to ortho. Make map from dims to set of hash, and map from origin to set of hash, map from hop to set of hash. intersect hashes and query master.
-- 2. make map from dims to orthos with those dims. linear search in there for right origin or hop as desired
-- 3. make map from dims to an opaque structure. This structure holds a map from origin to hashes and a map from hop to hashes. It also holds a map from hash to ortho.
-- 4. give up on nonduplication and make a map from dims to map from origin to ortho, and another map which is from dims to map from hop to ortho