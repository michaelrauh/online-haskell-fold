import Test.HUnit
import System.Exit
import Lib

import qualified Data.Map as Map
import qualified Data.Set as Set

inputString = "First, second. Third - fourth\nfifth. first third"

result = makeNextMapping inputString
expected = Map.fromList [("first", Set.fromList ["second", "third"]), ("third", Set.fromList ["fourth"]), ("fourth", Set.fromList ["fifth"])]
testNext = TestCase $ assertEqual "makes next mapping but drops across periods and strips caps and punctuation" expected result

expectedPrev = Map.fromList [("fifth", Set.fromList ["fourth"]), ("fourth", Set.fromList ["third"]), ("second", Set.fromList ["first"]), ("third", Set.fromList ["first"])]
resultPrev = makePrevMapping inputString
testPrev = TestCase $ assertEqual "makes prev mapping in the same style as next mapping" expectedPrev resultPrev

expectedPhrases = Set.fromList [["first"], ["second"], ["first", "second"], ["third"], ["fourth"], ["fifth"], ["third", "fourth"], ["fourth", "fifth"], ["third", "fourth", "fifth"], ["first", "third"]]
resultPhrases = makePhrases inputString
testPhrases = TestCase $ assertEqual "makes phrase set" expectedPhrases resultPhrases

expectedVocabulary = ["fifth", "first", "fourth", "second", "third"]
resultVocabulary = makeVocabulary inputString
testVocabulary = TestCase $ assertEqual "makes vocab list" expectedVocabulary resultVocabulary

testlist = TestList [TestLabel "nextMapping" testNext,
                    TestLabel "prevMapping" testPrev,
                    TestLabel "phrases" testPhrases,
                    TestLabel "vocabulary" testVocabulary
                    ]

main :: IO ()
main = do
  results <- runTestTT testlist
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)