import Test.HUnit
import System.Exit
import Lib

import qualified Data.Map as Map
import qualified Data.Set as Set

result = makeNextMapping "First, second. Third - fourth\nfifth. first third"
expected = Map.fromList [("first", Set.fromList ["second", "third"]), ("third", Set.fromList ["fourth"]), ("fourth", Set.fromList ["fifth"])]
testNext = TestCase $ assertEqual "makes next mapping but drops across periods and strips caps and punctuation" expected result

testlist = TestList [TestLabel "nextMapping" testNext
                    ]

main :: IO ()
main = do
  results <- runTestTT testlist
  if errors results + failures results == 0
    then
      exitSuccess
    else
      exitWith (ExitFailure 1)