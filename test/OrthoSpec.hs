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
    it "is equal if the middle is switched" $ do
      let left = fromAnswer $ Answer "a" "b" "c" "d"
          right = fromAnswer $ Answer "a" "c" "b" "d"
      left `shouldBe` right