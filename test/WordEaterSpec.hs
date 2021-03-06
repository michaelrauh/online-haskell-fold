{-# LANGUAGE OverloadedStrings #-}

module WordEaterSpec (spec) where

import WordEater ( wordToUniqueAnswer, Answer(Answer) )
import Config ( makeConfig )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (pack)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "WordEater" $ do
    it "eats words to answers" $ do
      let result = wordToUniqueAnswer (makeConfig "a b c d a c b d") "d"
          expected = [Answer "a" "b" "c" "d"]
      result `shouldBe` expected