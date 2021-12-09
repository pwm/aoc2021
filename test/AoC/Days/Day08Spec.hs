module AoC.Days.Day08Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day08
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 08)
    parse
    (solveA, 479)
    (solveB, 1041746)
