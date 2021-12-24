module AoC.Days.Day24Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day24
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 24)
    parse
    (solveA, ())
    (solveB, ())
