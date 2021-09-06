module Orthos where
import Ortho
    (
      Ortho,
      fromAnswer,
      isNotBase,
      getOrigin,
      getName, getNodesOfDistanceGreaterThan, getHop, DirectedOrtho )
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
      Set )
import Data.Map as Map ( findWithDefault, fromList )
import Data.Text ( unpack )
import WordEater (Answer (..), wordToUniqueAnswer)
import Data.List ( permutations )

newtype Orthos = Orthos (Set.Set Ortho)
data Correspondence = Correspondence {fromOrtho :: Ortho, toOrtho :: Ortho, corr :: [(String, String)]}
newtype DirectedOrthos = DirectedOrthos (Set.Set DirectedOrtho)

-- TODO index orthos by origin

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

selectSameDimensionalityForBaseOrtho :: Orthos -> Ortho -> Orthos -- this assumes ortho is base dims and only selects based upon number of dims
selectSameDimensionalityForBaseOrtho = error "not implemented"

filterOld :: Orthos -> Orthos -> Orthos
filterOld (Orthos old) (Orthos new) = Orthos $ new `Set.difference` old

toList :: Orthos -> [Ortho]
toList (Orthos s) = Set.toList s

findWithOrigin :: Orthos -> Set String -> Orthos -- this would be faster if orthos were indexed by origin
findWithOrigin = undefined