module WordEaterSpec (spec) where

import WordEater 
import Config
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "WordEater" $ do
    it "eats words to answers" $ do
      let result = wordToUniqueAnswer (makeConfig "a b c d a c b d") "d"
          expected = [Answer "a" "b" "c" "d"]
      result `shouldBe` expected