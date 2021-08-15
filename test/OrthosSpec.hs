module OrthosSpec (spec) where

import Config (makeConfig)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Ortho (eatWord)
import Orthos

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Orthos" $ do
    it "allows insertion of an ortho and reflects with greater size" $ do
      let ortho = head $ eatWord (makeConfig "a b c d a c b d") "d"
          result = size $ insert empty ortho
          expected = 1
      result `shouldBe` expected