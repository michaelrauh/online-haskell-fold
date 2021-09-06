module Fold where
import Orthos
import Config 
import Ortho
import Data.Set as Set
import Data.Map as Map
import WordEater
import Data.List
import Data.Text

digestWords :: Config -> Orthos
digestWords config = Prelude.foldr (digestWord config) emptyOrtho $ vocab config

digestWord :: Config -> String -> Orthos -> Orthos
digestWord config word orthos = sift config orthos $ eatWord config word

sift :: Config -> Orthos -> Orthos -> Orthos
sift config known increment =
  if isEmpty increment
    then known
    else
      let (f, rest) = pickOne increment
          incrementUp = combineUpIfBase config known f
          newKnown = mergeOrthos known incrementUp
          up = sift config newKnown incrementUp
          incrementOver = combineOver config up f
          veryNewKnown = mergeOrthos newKnown incrementOver
          over = sift config veryNewKnown incrementOver
       in sift config over rest

eatWord :: Config -> String -> Orthos
eatWord conf cur = Orthos $ Set.fromList $ fromAnswer <$> wordToUniqueAnswer conf cur

checkNodeProjectsForwardAcrossMappedPath :: Config -> Map String String -> Set Node -> Set Node -> Bool -- reader would jump config over this 
checkNodeProjectsForwardAcrossMappedPath c corrMap from to =
  let correspondingNodeNames = findCorresponding corrMap to <$> Set.toList from
   in checkEachPairProjects c correspondingNodeNames

checkEachPairProjects :: Config -> [(String, String)] -> Bool -- this would be faster if text
checkEachPairProjects  = undefined

checkAllProjectionsExceptOriginAndHop :: Config -> Correspondence -> Bool -- reader would jump config over this
checkAllProjectionsExceptOriginAndHop c (Correspondence from to corr) =
  let corrMap = Map.fromList corr
      nodesToCheckFrom = getNodesOfDistanceGreaterThan 1 from
      nodesToCheckTo = getNodesOfDistanceGreaterThan 1 to
      checksPassed = checkNodeProjectsForwardAcrossMappedPath c corrMap nodesToCheckTo nodesToCheckFrom
   in checksPassed

findAxisCorrespondence :: Config -> Ortho -> Ortho -> [Correspondence] -- there are less redundant ways to do this. If one axis fails it's a complete failure.
findAxisCorrespondence c from to =
  let corrPerms = uncurry Prelude.zip <$> ((,) <$> permutations (Set.toList $ getHop from) <*> permutations (Set.toList $ getHop to))
      finds = Prelude.filter (checkEachPairProjects c) corrPerms
   in Correspondence from to <$> finds

combineUpIfBase :: Config -> Orthos -> Ortho -> Orthos
combineUpIfBase c os o =
  if isNotBase o
    then emptyOrtho
    else
      let newOs = selectSameDimensionalityForBaseOrtho os o
          l = combineUpLeft c newOs o
          r = combineUpRight c newOs o
       in mergeOrthos l r

-- Reader would allow skipping this config pass
combineUpRight :: Config -> Orthos -> Ortho -> Orthos
combineUpRight c os o =
  let projects = projectsBackward c os o
      found = diagonalsLeft projects o
   in filterOld os found

combineUpLeft :: Config -> Orthos -> Ortho -> Orthos
combineUpLeft c os o =
  let projects = projectsForward c os o
      found = diagonalsRight projects o
   in filterOld os found

projectsForward :: Config -> Orthos -> Ortho -> Orthos
projectsForward c os o =
  let forward = Map.findWithDefault Set.empty ((unpack . getName . getOrigin) o) (next c) -- this unpack would be unneccesary if everything was text
      matchingOrigin = findWithOrigin os forward
      axisCorrespondence = Prelude.concat $ findAxisCorrespondence c o <$> Orthos.toList matchingOrigin
   in Orthos $ Set.fromList (toOrtho <$> Prelude.filter (checkAllProjectionsExceptOriginAndHop c) axisCorrespondence)

projectsBackward :: Config -> Orthos -> Ortho -> Orthos
projectsBackward = undefined

diagonalsLeft :: Orthos -> Ortho -> Orthos
diagonalsLeft = undefined

diagonalsRight :: Orthos -> Ortho -> Orthos
diagonalsRight = undefined

combineOver :: Config -> Orthos -> Ortho -> Orthos
combineOver = undefined

centersMatchLeft :: Orthos -> Ortho -> DirectedOrthos
centersMatchLeft = undefined

centersMatchRight :: Orthos -> Ortho -> DirectedOrthos
centersMatchRight = undefined

phrasesMatchLeft :: DirectedOrthos -> Ortho -> DirectedOrthos
phrasesMatchLeft = undefined

phrasesMatchRight :: DirectedOrthos -> Ortho -> DirectedOrthos
phrasesMatchRight = undefined

combineOverLeft :: DirectedOrthos -> Ortho -> Orthos
combineOverLeft = undefined

combineOverRight :: DirectedOrthos -> Ortho -> Orthos
combineOverRight = undefined