module AoC.Days.Day10Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day10
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 10)
    parse
    (solveA, 311949)
    (solveB, 3042730309)
