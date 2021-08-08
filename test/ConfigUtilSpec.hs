module ConfigUtilSpec (spec) where

import Test.Hspec
import System.Exit
import ConfigUtil

import qualified Data.Map as Map
import qualified Data.Set as Set

{-# ANN module "HLint: ignore Redundant do" #-}

inputString = "First, second. Third - fourth\nfifth. first third"

spec :: Spec
spec = do
    describe "ConfigUtil" $ do
        it "makes a next mapping" $ do
            let result = makeNextMapping inputString
                expected = Map.fromList [("first", Set.fromList ["second", "third"]), ("third", Set.fromList ["fourth"]), ("fourth", Set.fromList ["fifth"])]
            result `shouldBe` expected
        
        it "makes a previous mapping" $ do 
            let result = makePrevMapping inputString 
                expected = Map.fromList [("fifth", Set.fromList ["fourth"]), ("fourth", Set.fromList ["third"]), ("second", Set.fromList ["first"]), ("third", Set.fromList ["first"])]
            result `shouldBe` expected
        
        it "makes phrases" $ do 
            let result = makePhrases inputString
                expected = Set.fromList [["first"], ["second"], ["first", "second"], ["third"], ["fourth"], ["fifth"], ["third", "fourth"], ["fourth", "fifth"], ["third", "fourth", "fifth"], ["first", "third"]]
            result `shouldBe` expected
        
        it "makes vocabulary" $ do 
            let result = makeVocabulary inputString
                expected = ["fifth", "first", "fourth", "second", "third"]
            result `shouldBe` expected