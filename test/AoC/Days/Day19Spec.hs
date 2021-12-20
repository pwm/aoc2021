module AoC.Days.Day19Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day19
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 19)
    parse
    (solveA, ())
    (solveB, ())
