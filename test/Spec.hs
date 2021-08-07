import Test.HUnit
import System.Exit
import Lib

import qualified Data.Map as Map
import qualified Data.Set as Set

result = makeNextMapping "a b"
expected = Map.fromList [("a", Set.fromList ["b"])]
testNext = TestCase $ assertEqual "makes next mapping" expected result

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