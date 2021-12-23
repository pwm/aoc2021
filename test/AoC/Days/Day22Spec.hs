module AoC.Days.Day22Spec
  ( spec,
  )
where

import AoC.Core.Day
import AoC.DayTester
import AoC.Days.Day22
import Test.Hspec

spec :: Spec
spec =
  dayTester
    (mkDay 22)
    parse
    (solveA, 580098)
    (solveB, ())
