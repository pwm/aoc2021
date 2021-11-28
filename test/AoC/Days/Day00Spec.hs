module AoC.Days.Day00Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day00
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 0)
    parse
    (solveA, ())
    (solveB, ())
