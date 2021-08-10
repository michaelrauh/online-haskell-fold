module OrthoSpec (spec) where

import Config (makeConfig)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Ortho

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Ortho" $ do
    it "exposes origin through ortho" $ do
      let result = getOrigin $ head $ eatWord (makeConfig "a b c d a c b d") "d"
          expected = "a"
      result `shouldBe` expected
    it "exposes dimensions through ortho" $ do
      let result = getDimensions $ head $ eatWord (makeConfig "a b c d a c b d") "d"
          expected = [1, 2, 1]
      result `shouldBe` expected
    it "allows seeing what is one away from origin" $ do
      let result = hop $ head $ eatWord (makeConfig "a b c d a c b d") "d"
          expected = Set.fromList ["b", "c"]
      result `shouldBe` expected
    it "allows unraveling based upon a hop direction" $ do
      let result = unravel (head $ eatWord (makeConfig "a b c d a c b d") "d") "b"
          expected = ["a", "b", "c", "d"]
      result `shouldBe` expected
    it "allows unraveling based upon a hop direction the other way" $ do
      let result = unravel (head $ eatWord (makeConfig "a b c d a c b d") "d") "c"
          expected = ["a", "c", "b", "d"]
      result `shouldBe` expected
    it "reveals diagonals" $ do
      let result = diagonals (head $ eatWord (makeConfig "a b c d a c b d") "d")
          expected = [Set.fromList ["a"], Set.fromList ["c", "b"], Set.fromList ["d"]]
      result `shouldBe` expected
    it "allows unraveling based upon a hop direction for the lhs center" $ do
      let result = lhsCenter (head $ eatWord (makeConfig "a b c d a c b d") "d") "c"
          expected = ["a", "b"]
      result `shouldBe` expected