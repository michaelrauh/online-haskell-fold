{-# LANGUAGE OverloadedStrings #-}

module OrthoSpec (spec) where

import Config (makeConfig)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec (Spec, describe, it, shouldBe)
import Ortho
import WordEater

spec :: Spec
spec = do
  describe "Ortho" $ do
    it "allows creating from an answer and pretty prints" $ do
      let left = show [["a"], ["b", "c"], ["d"]]
          right = (makePretty . fromAnswer) $ Answer "a" "b" "c" "d"
      left `shouldBe` right
    it "is equal if the middle is switched" $ do
      let left = fromAnswer $ Answer "a" "b" "c" "d"
          right = fromAnswer $ Answer "a" "c" "b" "d"
      left `shouldBe` right
    it "merges two orthos in an up direction" $ do
      let left = fromAnswer $ Answer "a" "b" "c" "d"
          right = fromAnswer $ Answer "e" "f" "g" "h"
          corr = (Correspondence left right (Map.fromList [("a", "e"), ("b", "f"), ("c", "g"), ("d", "h")]))
      makePretty (mergeUp corr) `shouldBe` show [["a"], ["b", "c", "e"], ["d", "f", "g"], ["h"]]