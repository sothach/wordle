module UnitTests where

import Test.HUnit
import Data.Set (fromList, empty)
import qualified Data.Set as Set
import Wordle
import Helpers

{-
λ :load WordleSpec.hs
λ runTestTT suite
-}
suite = TestList [TestLabel "First guess" guess1, TestLabel "Winning play" winningPlay]

guess1 = TestCase (do
                    go <- withInput (newGame "train") "crane"
                    let round = evaluate go
                    assertEqual "first guess fails" False (solved round)
                    assertEqual "matched letters"   "_ra__" (pattern round)
                    assertEqual "excludes letters"  "ce" (set2string $ excludes round)
                    assertEqual "misplaced letters" [empty,empty,empty,fromList("n"),empty] (misplaced round)
                  )

winningPlay = TestCase (do
                    go <- withInput (newGame "adieu") "adieu"
                    let round = evaluate go
                    assertEqual "first guess succeeds" True (solved round)
                    assertEqual "all matched letters"  "adieu" (pattern round)
                    assertEqual "no excludes letters"  empty (excludes round)
                    assertBool  "no misplaced letters" (and (map (\el -> el == empty) (misplaced round)))
                  )