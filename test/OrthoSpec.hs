module OrthoSpec (spec) where

import Config (makeConfig)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Ortho (eatWord, empty, findWithMatchingDimsAndHopToOrigin, findWithMatchingDimsAndOriginProjectingBackward, findWithMatchingDimsAndOriginProjectingForward, findWithMatchingDimsAndOriginToHop, insert, size)

{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

spec :: Spec
spec = do
  describe "Ortho" $ do
    it "creating orthos and inserting them in multiple indices equivalent to dimensionality + 1" $ do
      let result = size $ insert empty (head $ eatWord (makeConfig "a b c d a c b d") "d")
          expected = 3
      result `shouldBe` expected
    it "searching for orthos by dims and origin over a projection when looking for RHS" $ do
      let config = makeConfig "a b c d a c b d a e f g h e g f h"
          original = head $ eatWord config "d"
          new = head $ eatWord config "h"
          store = insert empty new
          result = findWithMatchingDimsAndOriginProjectingForward config store original
          expected = Set.singleton new
      result `shouldBe` expected
    it "searching for orthos by dims and origin over a projection when looking for LHS" $ do
      let config = makeConfig "a b c d a c b d a e f g h e g f h"
          new = head $ eatWord config "d"
          original = head $ eatWord config "h"
          store = insert empty new
          result = findWithMatchingDimsAndOriginProjectingBackward config store original
          expected = Set.singleton new
      result `shouldBe` expected
    it "searching for orthos by dims by matching a hop to an origin RHS" $ do
      let config = makeConfig "a b c d a c b d b e d f b d e f"
          new = head $ eatWord config "f"
          original = head $ eatWord config "d"
          store = insert empty new
          result = findWithMatchingDimsAndHopToOrigin store original
          expected = Set.singleton new
      result `shouldBe` expected
    it "searching for orthos by dims by matching an origin to a hop LHS" $ do
      let config = makeConfig "a b c d a c b d b e d f b d e f"
          original = head $ eatWord config "f"
          new = head $ eatWord config "d"
          store = insert empty new
          result = findWithMatchingDimsAndOriginToHop store original
          expected = Set.singleton new
      result `shouldBe` expected
    it "checks to make sure diagonals don't match up for all rotations. Has a left to right bias" $ do
      let config = makeConfig "a b c d a c b d b e d f b d e f"
          original = head $ eatWord config "f"
          new = head $ eatWord config "d"
          store = insert empty new
          result = findWithMatchingDimsAndOriginToHop store original
          expected = Set.singleton new
      result `shouldBe` expected

-- a b  
-- c d  