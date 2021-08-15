module Orthos (empty, insert, size) where
import Ortho
import Data.Set as Set (Set, fromList, toList)

newtype Orthos = Orthos [Ortho]

empty :: Orthos 
empty = Orthos []

insert :: Orthos -> Ortho -> Orthos
insert (Orthos os) ortho = Orthos $ ortho : os

size :: Orthos -> Int
size (Orthos l) = length l

findByDimsAndOrigin :: Orthos -> [Int] -> String -> Set.Set Ortho
findByDimsAndOrigin orthos dims origin = error "not implemented"

findByDimsAndHop :: Orthos -> [Int] -> [String] -> Set.Set Ortho
findByDimsAndHop orthos dims hop = error "not implemented"