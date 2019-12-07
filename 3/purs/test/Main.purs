module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Main (Direction(..), Move(..), Point(..), allPoints, moves)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest suites

suites :: TestSuite
suites = do
  suite "day 3" do
    test "generate all moves 1" $
      --Assert.equal [(Point 1 0)] (allPoints "R1")
      Assert.equal [(Move Right 1)] (moves "R1")

    test "generate all moves 2" $
      --Assert.equal [(Point 1 0), (Point 1 1)] (allPoints "R1,U1")
      Assert.equal [(Move Right 1), (Move Up 1)] (moves "R1,U1")

    test "generate all moves 3" $
      --Assert.equal [(Point 1 0), (Point 1 1)] (allPoints "R1,U1,D1")
      Assert.equal [(Move Right 1), (Move Up 11), (Move Down 1)] (moves "R1,U11,D1")